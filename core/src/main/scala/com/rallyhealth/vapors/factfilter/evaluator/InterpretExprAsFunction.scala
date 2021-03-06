package com.rallyhealth.vapors.factfilter.evaluator

import cats._
import cats.data.Chain
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.algebra.{ConditionBranch, Expr, ExprResult}
import com.rallyhealth.vapors.core.logic._
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.{Input, Output}

import scala.collection.immutable.BitSet

// TODO: Make R the last parameter?
final class InterpretExprAsFunction[F[_] : Foldable, V, P]
  extends Expr.Visitor[F, V, P, Lambda[r => Input[F, V] => ExprResult[F, V, r, P]]] {

  import cats.syntax.all._
  import com.rallyhealth.vapors.core.syntax.math._

  override def visitAddOutputs[R : Addition](
    expr: Expr.AddOutputs[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val inputResultList = inputResults.toList
    val outputValue = inputResultList.map(_.output.value).reduceLeft(_ + _)
    val allEvidence = inputResultList.foldMap(_.output.evidence)
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, outputValue, allEvidence, allParams) {
      ExprResult.AddOutputs(_, _, inputResultList)
    }
  }

  override def visitAnd[R : Conjunction : ExtractBoolean](
    expr: Expr.And[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val combinedOutput = inputResults.map(_.output).reduceLeft(Conjunction[Output[R]].conjunction)
    val inputResultList = inputResults.toList
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, combinedOutput.value, combinedOutput.evidence, allParams) {
      ExprResult.And(_, _, inputResultList)
    }
  }

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val collectFn = expr.collectExpr.visit(InterpretExprAsFunction())
    val (combinedResult, combinedEvidence, allParams) = inputResult.output.value.collectFoldSome { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val collectInput = input.withValue(elem, inputEvidence)
      val collectResult = collectFn(collectInput)
      // TODO: Keep all params / replay steps instead of just the ones that return true?
      collectResult.output.value.map { result =>
        (
          result,
          collectResult.output.evidence,
          collectResult.param :: Nil,
        )
      }
    }
    resultOfManySubExpr(expr, input, combinedResult, combinedEvidence, allParams) {
      ExprResult.CollectFromOutput(_, _, inputResult)
    }
  }

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Input[F, V] => ExprResult[F, V, R, P] = {
    input =>
      resultOfPureExpr(expr, input, expr.value, input.evidence) {
        ExprResult.ConstOutput(_, _)
      }
  }

  override def visitDefine[M[_] : Foldable, T](
    expr: Expr.Define[M, T, P],
  ): Input[F, V] => ExprResult[F, V, FactSet, P] = { input =>
    val definitionFn = expr.definitionExpr.visit(InterpretExprAsFunction())
    val definitionContext = input.withValue(input.factTable)
    val definitionResult = definitionFn(definitionContext)
    val definedFactSet = definitionResult.output.value.foldMap { definitionValue =>
      FactSet(DerivedFact(expr.factType, definitionValue, definitionResult.output.evidence))
    }
    val output = Output(definedFactSet, definitionResult.output.evidence)
    val postParam = expr.capture.foldToParam(expr, definitionContext, output, definitionResult.param :: Nil)
    ExprResult.Define(expr, ExprResult.Context(input, output, postParam), definitionResult)
  }

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val embeddedFn = expr.embeddedExpr.visit(InterpretExprAsFunction())
    val embeddedInput = input.withValue(input.factTable)
    val embeddedResult = embeddedFn(embeddedInput)
    // The evidence of the surrounding context is not relevant to the evidence of the embedded expression
    // so we ignore it and leave it up to the parent expression to combine evidence as it sees fit.
    resultOfSingleSubExpr(expr, input, embeddedResult) {
      ExprResult.Embed(_, _, embeddedResult)
    }
  }

  override def visitExistsInOutput[M[_] : Foldable, U](
    expr: Expr.ExistsInOutput[F, V, M, U, P],
  ): Input[F, V] => ExprResult[F, V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val conditionFn = expr.conditionExpr.visit(InterpretExprAsFunction())
    val (allMatchedIndexes, allEvidence, allCondResults) = inputResult.output.value.toList.zipWithIndex.collectFold {
      case (elem, idx) =>
        // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
        val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
        val conditionInput = input.withValue(elem, inputEvidence)
        val conditionResult = conditionFn(conditionInput)
        val isMatch = conditionResult.output.value
        val (matchedIdx, accEvidence, accResults) = {
          if (isMatch) (Chain(idx), conditionResult.output.evidence, Chain(conditionResult))
          else (Chain.nil, Evidence.none, Chain.nil)
        }
        (matchedIdx, accEvidence, accResults)
    }
    val matchedIndexSet = allMatchedIndexes.iterator.to(BitSet)
    val condResultList = allCondResults.toList
    val allParams = condResultList.map(_.param)
    // TODO: Better way to capture the param from the inputResult separate from the condResultList params?
    resultOfManySubExpr(expr, input, matchedIndexSet.nonEmpty, allEvidence, inputResult.param :: allParams) {
      ExprResult.ExistsInOutput(_, _, inputResult, condResultList, matchedIndexSet)
    }
  }

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, U](
    expr: Expr.FilterOutput[F, V, M, U, P],
  ): Input[F, V] => ExprResult[F, V, M[U], P] = { input =>
    implicit val functorM: Functor[M] = FunctorFilter[M].functor
    val inputResult = expr.inputExpr.visit(this)(input)
    val condFn = expr.condExpr.visit(InterpretExprAsFunction())
    val condResults = inputResult.output.value.map { elem =>
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val condInput = input.withValue(elem, inputEvidence)
      condFn(condInput)
    }
    val matchingValues = condResults.collect {
      case condResult if condResult.output.value => condResult.input.value
    }
    val (matchingEvidence, matchingParams) = condResults.collectFoldSome { result =>
      Option.when(result.output.value) {
        (result.output.evidence, result.param :: Nil)
      }
    }
    val condResultList = condResults.toList
    // TODO: Better way to capture the param from the inputResult separate from the condResultList params?
    resultOfManySubExpr(expr, input, matchingValues, matchingEvidence, inputResult.param :: matchingParams) {
      ExprResult.FilterOutput(_, _, inputResult, condResultList)
    }
  }

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[F, V, M, U, X, P],
  ): Input[F, V] => ExprResult[F, V, M[X], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val flatMapFn = expr.flatMapExpr.visit(InterpretExprAsFunction())
    val allResults = inputResult.output.value.map { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val flatMapInput = input.withValue(elem, inputEvidence)
      flatMapFn(flatMapInput)
    }
    val allValues = allResults.flatMap(_.output.value)
    // TODO: Shouldn't I only look at the evidence of results that aren't empty?
    val (allEvidence, allParams) = allResults.foldMap { elemResult =>
      (elemResult.output.evidence, elemResult.param :: Nil)
    }
    val subOps = allResults.toList
    resultOfManySubExpr(expr, input, allValues, allEvidence, allParams) {
      ExprResult.FlatMapOutput(_, _, inputResult, subOps)
    }
  }

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](
    expr: Expr.MapOutput[F, V, M, U, R, P],
  ): Input[F, V] => ExprResult[F, V, M[R], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val mapFn = expr.mapExpr.visit(InterpretExprAsFunction())
    val allResults = inputResult.output.value.map { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val mapInput = input.withValue(elem, inputEvidence)
      mapFn(mapInput)
    }
    val allValues = allResults.map(_.output.value)
    val (allEvidence, allParams) = allResults.foldMap { elemResult =>
      (elemResult.output.evidence, elemResult.param :: Nil)
    }
    val subOps = allResults.toList
    resultOfManySubExpr(expr, input, allValues, allEvidence, allParams) {
      ExprResult.MapOutput(_, _, inputResult, subOps)
    }
  }

  override def visitNegativeOutput[R : Negative](
    expr: Expr.NegativeOutput[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val outputValue = Negative[R].negative(inputResult.output.value)
    resultOfManySubExpr(expr, input, outputValue, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.NegativeOutput(_, _, inputResult)
    }
  }

  override def visitNot[R : Negation](expr: Expr.Not[F, V, R, P]): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val outputValue = Negation[R].negation(inputResult.output.value)
    resultOfManySubExpr(expr, input, outputValue, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.Not(_, _, inputResult)
    }
  }

  override def visitOr[R : Disjunction : ExtractBoolean](
    expr: Expr.Or[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val combinedOutput = inputResults.map(_.output).reduceLeft(Disjunction[Output[R]].disjunction)
    val inputResultList = inputResults.toList
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, combinedOutput.value, combinedOutput.evidence, allParams) {
      ExprResult.Or(_, _, inputResultList)
    }
  }

  override def visitOutputIsEmpty[M[_] : Foldable, R](
    expr: Expr.OutputIsEmpty[F, V, M, R, P],
  ): Input[F, V] => ExprResult[F, V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isEmpty = inputResult.output.value.isEmpty
    resultOfPureExpr(expr, input, isEmpty, inputResult.output.evidence) {
      ExprResult.OutputIsEmpty(_, _, inputResult)
    }
  }

  override def visitOutputWithinSet[R](
    expr: Expr.OutputWithinSet[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isWithinSet = expr.accepted.contains(inputResult.output.value)
    resultOfPureExpr(expr, input, isWithinSet, inputResult.output.evidence) {
      ExprResult.OutputWithinSet(_, _, inputResult)
    }
  }

  override def visitOutputWithinWindow[R](
    expr: Expr.OutputWithinWindow[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isWithinWindow = expr.window.contains(inputResult.output.value)
    resultOfPureExpr(expr, input, isWithinWindow, inputResult.output.evidence) {
      ExprResult.OutputWithinWindow(_, _, inputResult)
    }
  }

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Input[F, V] => ExprResult[F, V, F[V], P] = { input =>
    resultOfPureExpr(expr, input, input.value, input.evidence ++ Evidence.fromAnyOrNone(input.value)) {
      ExprResult.ReturnInput(_, _)
    }
  }

  override def visitSelectFromOutput[S, R](
    expr: Expr.SelectFromOutput[F, V, S, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val selected = expr.lens.get(inputResult.output.value)
    resultOfManySubExpr(expr, input, selected, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.SelectFromOutput(_, _, inputResult)
    }
  }

  override def visitSubtractOutputs[R : Subtraction](
    expr: Expr.SubtractOutputs[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val allResults = expr.inputExprList.map(_.visit(this)(input))
    val allResultsList = allResults.toList
    val addResult = allResultsList.map(_.output.value).reduceLeft(_ - _)
    val allEvidence = allResultsList.foldMap(_.output.evidence)
    val allParams = allResultsList.map(_.param)
    resultOfManySubExpr(expr, input, addResult, allEvidence, allParams) {
      ExprResult.SubtractOutputs(_, _, allResultsList)
    }
  }

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[F, V, M, R, P],
  ): Input[F, V] => ExprResult[F, V, M[R], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val values = inputResult.output.value
    val takeWindow: Window[Int] = expr.take match {
      case pos if pos > 0 => Window.between(0, pos)
      case 0 => Window.empty
      case neg if neg < 0 =>
        val totalSize = values.size.toInt
        Window.between(totalSize + neg, totalSize)
    }
    // TODO: Handle evidence when the input is a set / sequence of facts
    val selectedValues = values.zipWithIndex.collect {
      case (elem, idx) if takeWindow.contains(idx) => elem
    }
    resultOfManySubExpr(expr, input, selectedValues, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.TakeFromOutput(_, _, inputResult)
    }
  }

  override def visitUsingDefinitions[R](
    expr: Expr.UsingDefinitions[F, V, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val definitionVisitor = new InterpretExprAsFunction[Id, FactTable, P]
    val definitionInput = input.withValue(input.factTable)
    val (declaredFacts, evidence, declaredParams) = expr.definitions.foldMap { defExpr =>
      val definitionFn = defExpr.visit(definitionVisitor)
      val definitionResult = definitionFn(definitionInput)
      (definitionResult.output.value, definitionResult.output.evidence, definitionResult.param :: Nil)
    }
    val subInput = input.copy(factTable = input.factTable.addAll(declaredFacts))
    val subFn = expr.subExpr.visit(this)
    val subResult = subFn(subInput)
    // TODO: Come up with a better way to combine the CaptureP params from expressions that have multiple
    //       sub expressions with different meanings.
    val allParams = subResult.param :: declaredParams
    val postParam = expr.capture.foldToParam(expr, input, subResult.output, allParams)
    ExprResult.UsingDefinitions(expr, ExprResult.Context(input, subResult.output, postParam), subResult)
  }

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    val (maybeConditionResult, condParams) = {
      expr.conditionBranches.foldLeft((None, Nil): (Option[(ConditionBranch[F, V, R, P], Evidence)], List[Eval[P]])) {
        case (acc @ (Some(_), _), _) => acc
        case ((None, params), cond) =>
          val whenResult = cond.whenExpr.visit(this)(input)
          val conditionMet = whenResult.output.value
          (Option.when(conditionMet)((cond, whenResult.output.evidence)), whenResult.param :: params)
      }
    }
    val (thenExpr, condEvidence) = maybeConditionResult
      .map {
        case (branch, evidence) => (branch.thenExpr, evidence)
      }
      .getOrElse {
        (expr.defaultExpr, Evidence.none)
      }
    val thenResult = thenExpr.visit(this)(input)
    // union the evidence for the condition with the evidence for the output
    val allEvidence = condEvidence.union(thenResult.output.evidence)
    // TODO: Better way to organize the params than to just put the thenExpr param at the head?
    val allParams = thenResult.param :: condParams
    resultOfManySubExpr(expr, input, thenResult.output.value, allEvidence, allParams) {
      ExprResult.When(_, _, maybeConditionResult.map(_._1), thenResult)
    }
  }

  override def visitWithFactsOfType[T, R](
    expr: Expr.WithFactsOfType[T, R, P],
  ): Input[F, V] => ExprResult[F, V, R, P] = { input =>
    import alleycats.std.set._
    val inputFactTable = input.withValue(input.factTable)
    val withMatchingFactsFn = expr.subExpr.visit(InterpretExprAsFunction())
    val matchingFacts = input.factTable.getSortedSeq(expr.factTypeSet)
    // facts will always be added as their own evidence when used, so we do not need to add them to the evidence here
    val subInput = input.withFoldableValue[Seq, TypedFact[T]](matchingFacts)
    val subResult = withMatchingFactsFn(subInput)
    val postParam = expr.capture.foldToParam(expr, inputFactTable, subResult.output, subResult.param :: Nil)
    ExprResult.WithFactsOfType(
      expr,
      ExprResult.Context(input, subResult.output, postParam),
      subResult,
    )
  }

  @inline private def resultOfPureExpr[R](
    expr: Expr[F, V, R, P],
    input: Input[F, V],
    result: R,
    evidence: Evidence,
  )(
    buildPostOp: (expr.type, ExprResult.Context[F, V, R, P]) => ExprResult[F, V, R, P],
  ): ExprResult[F, V, R, P] = {
    resultOfManySubExpr(expr, input, result, evidence, Nil)(buildPostOp)
  }

  @inline private def resultOfManySubExpr[R](
    expr: Expr[F, V, R, P],
    input: Input[F, V],
    result: R,
    evidence: Evidence,
    capturedParams: List[Eval[P]],
  )(
    buildResult: (expr.type, ExprResult.Context[F, V, R, P]) => ExprResult[F, V, R, P],
  ): ExprResult[F, V, R, P] = {
    val output = Output(result, evidence)
    val param = expr.capture.foldToParam(expr, input, output, capturedParams)
    buildResult(expr, ExprResult.Context(input, output, param))
  }

  // This takes a higher-kinded parameter G[_] because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details
  @inline private def resultOfSingleSubExpr[G[_], R](
    expr: Expr[F, V, R, P],
    input: Input[F, V],
    subResult: ExprResult[G, _, R, P],
  )(
    buildPostOp: (expr.type, ExprResult.Context[F, V, R, P]) => ExprResult[F, V, R, P],
  ): ExprResult[F, V, R, P] = {
    resultOfManySubExpr(expr, input, subResult.output.value, subResult.output.evidence, subResult.param :: Nil) {
      buildPostOp
    }
  }

}

object InterpretExprAsFunction {

  final def apply[F[_] : Foldable, V, P](): InterpretExprAsFunction[F, V, P] = new InterpretExprAsFunction

  final def apply[F[_] : Foldable, V, R, P](expr: Expr[F, V, R, P])(input: Input[F, V]): ExprResult[F, V, R, P] = {
    expr.visit(InterpretExprAsFunction())(input)
  }

  final case class Input[F[_], V](
    value: F[V],
    evidence: Evidence,
    factTable: FactTable,
  ) {

    @inline def withFoldableValue[G[_], U](
      value: G[U],
      evidence: Evidence = this.evidence,
    ): Input[G, U] = copy(value = value, evidence = evidence)

    @inline def withValue[U](
      value: U,
      evidence: Evidence = this.evidence,
    ): Input[Id, U] = copy[Id, U](value = value, evidence = evidence)
  }

  final object Input {

    type Init = Input[Id, FactTable]

    @inline def fromFactTable(factTable: FactTable): Init =
      Input[Id, FactTable](factTable, Evidence.none, factTable)

    @inline def fromValue[V](
      value: V,
      evidence: Evidence,
      factTable: FactTable,
    ): Input[Id, V] =
      Input[Id, V](value, evidence, factTable)

    @inline def fromValue[V](
      value: V,
      evidence: Evidence = Evidence.none,
    ): Input[Id, V] =
      fromValue(value, evidence, FactTable(evidence.factSet))
  }

  final case class Output[R](
    value: R,
    evidence: Evidence,
  )

  object Output {

    /**
      * Logical Conjunction (aka AND)
      *
      * - If either side is false, the result is false
      * - Evidence of falseness is accumulated
      * - Evidence of truthiness requires evidence of truth on both sides, otherwise no evidence of truth
      *
      * Examples:
      *
      * | X is True | Evidence of X | Y is True | Evidence of Y | Result is True | Evidence of Result |
      * | --------- | ------------- | --------- | ------------- | -------------- | ------------------ |
      * | T         | {}            | T         | {}            | T              | {}                 |
      * | T         | {}            | F         | {}            | F              | {}                 |
      * | T         | {}            | T         | {A}           | T              | {}                 |
      * | T         | {}            | F         | {A}           | F              | {A}                |
      * | T         | {B}           | T         | {A}           | T              | {A, B}             |
      * | T         | {B}           | F         | {}            | F              | {}                 |
      * | T         | {B}           | T         | {}            | T              | {}                 |
      * | T         | {B}           | F         | {A}           | F              | {A}                |
      * | F         | {}            | T         | {A}           | F              | {}                 |
      * | F         | {B}           | F         | {A}           | F              | {A, B}             |
      * | F         | {B}           | T         | {A}           | F              | {B}                |
      * | F         | {B}           | F         | {}            | F              | {B}                |
      * | F         | {}            | F         | {A}           | F              | {}                 |
      */
    implicit def conjunction[R : Conjunction : ExtractBoolean]: Conjunction[Output[R]] =
      (lhs: Output[R], rhs: Output[R]) => {
        import cats.syntax.apply._
        val R = ExtractBoolean[R]
        @inline def isTrue(output: Output[R]): Boolean = R.isTrue(output.value)
        val value = Conjunction[R].conjunction(lhs.value, rhs.value)
        val evidence = {
          if (R.isTrue(value)) {
            // only combine evidence of truthiness if both sides are true
            val evTrueL = Option.when(isTrue(lhs))(lhs.evidence)
            val evTrueR = Option.when(isTrue(rhs))(rhs.evidence)
            (evTrueL, evTrueR).mapN(_ ++ _).getOrElse(Evidence.none)
          } else {
            val evFalseL = Option.unless(isTrue(lhs))(lhs.evidence)
            val evFalseR = Option.unless(isTrue(rhs))(rhs.evidence)
            // combine any evidence of falseness
            (evFalseL ++ evFalseR).foldLeft(Evidence.none) { case (l, r) => l ++ r }
          }
        }
        Output(value, evidence)
      }

    /**
      * Logical Disjunction (aka inclusive OR)
      *
      * - If either side is true, the result is true
      * - Evidence of truthiness is accumulated
      * - Evidence of falseness requires evidence of false on both sides, otherwise there is no evidence of false
      *
      * Examples:
      *
      * | X is True | Evidence of X | Y is True | Evidence of Y | Result is True | Evidence of Result |
      * | --------- | ------------- | --------- | ------------- | -------------- | ------------------ |
      * | T         | {}            | T         | {}            | T              | {}                 |
      * | T         | {}            | F         | {}            | T              | {}                 |
      * | T         | {}            | T         | {A}           | T              | {A}                |
      * | T         | {}            | F         | {A}           | T              | {}                 |
      * | T         | {B}           | T         | {A}           | T              | {A, B}             |
      * | T         | {B}           | F         | {}            | T              | {B}                |
      * | T         | {B}           | T         | {}            | T              | {B}                |
      * | T         | {B}           | F         | {A}           | T              | {B}                |
      * | F         | {}            | T         | {A}           | T              | {A}                |
      * | F         | {B}           | F         | {A}           | F              | {A, B}             |
      * | F         | {B}           | T         | {A}           | T              | {A}                |
      * | F         | {B}           | F         | {}            | F              | {}                 |
      * | F         | {}            | F         | {A}           | F              | {}                 |
      */
    implicit def disjunction[R : Disjunction : ExtractBoolean]: Disjunction[Output[R]] =
      (lhs: Output[R], rhs: Output[R]) => {
        import cats.syntax.apply._
        val R = ExtractBoolean[R]
        @inline def isTrue(output: Output[R]): Boolean = R.isTrue(output.value)
        val value = Disjunction[R].disjunction(lhs.value, rhs.value)
        val evidence = {
          if (R.isTrue(value)) {
            // combine all evidence of truthiness from sides that are truthy
            val evTrueL = Option.when(isTrue(lhs))(lhs.evidence)
            val evTrueR = Option.when(isTrue(rhs))(rhs.evidence)
            (evTrueL ++ evTrueR).foldLeft(Evidence.none) { case (l, r) => l ++ r }
          } else {
            // only combine evidence of falseness if both sides are false
            val evFalseL = Option.unless(lhs.evidence.isEmpty)(lhs.evidence)
            val evFalseR = Option.unless(rhs.evidence.isEmpty)(rhs.evidence)
            (evFalseL, evFalseR).mapN(_ ++ _).getOrElse(Evidence.none)
          }
        }
        Output(value, evidence)
      }

    implicit def negation[R : Negation]: Negation[Output[R]] = { output =>
      val negatedValue = Negation[R].negation(output.value)
      output.copy(value = negatedValue)
    }

    /**
      * Combine two monoidal values and union their evidence.
      *
      * @note This is not _always_ safe. There may be some combinations of values in which you must combine
      *       [[Evidence]] for the resulting value differently based on the inputs. However, this is not the
      *       general purpose or intent of saying something is a [[Monoid]], so this definition should be
      *       generally safe. For example, there is no standard definition of [[Monoid]] for Boolean, because
      *       there is no safe assumption for and "empty" boolean value. However, evidence of true || true is
      *       not necessarily the same as evidence of true && true. Any definitions for which this distinction
      *       matters will typically use its own typeclasses, such as [[Conjunction]] or [[Disjunction]].
      */
    implicit def monoid[A : Monoid]: Monoid[Output[A]] = {
      new Monoid[Output[A]] {

        override final def empty: Output[A] = {
          Output(Monoid[A].empty, Evidence.none)
        }

        override final def combine(
          x: Output[A],
          y: Output[A],
        ): Output[A] = {
          Output(
            Monoid[A].combine(x.value, y.value),
            x.evidence ++ y.evidence,
          )
        }
      }
    }
  }

}
