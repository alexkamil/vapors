package com.rallyhealth.vapors.core.evaluator

import cats.free.FreeApplicative
import cats.instances.function._
import cats.~>
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.logic.{Intersect, Union}

private[evaluator] final class EvalLoop[O, T] extends (ExpAlg[O, T, *] ~> (((O, T)) => *)) {

  override def apply[A](fa: ExpAlg[O, T, A]): ((O, T)) => A = evalF(fa, _)

  private def evalLoop[U, B](exp: FreeApplicative[ExpAlg[O, U, *], B]): ((O, U)) => B = {
    exp.foldMap(this.asInstanceOf[EvalLoop[O, U]])
  }

  private def evalF[B](
    exp: ExpAlg[O, T, B],
    input: (O, T),
  ): B = {
    val (initData, data) = input
    exp match {
      case ExpAlg.Pure(_, value) =>
        value(data)
      case ExpAlg.Select(selector, expression) =>
        evalLoop(expression)(initData, selector.get(data))
      case ExpAlg.Exists(toIterable, condition, whenTrue, whenFalse) =>
        // we don't need this intermediate list and can remove it for performance,
        // but for the time being this is a good place to put a debugger
        val results = toIterable(data).iterator.map((initData, _)).map(evalLoop(condition)).toList
        val success = Union[Boolean].union(results)
        if (success) whenTrue(data)
        else whenFalse(data)
      case ExpAlg.ForAll(toIterable, condition, whenTrue, whenFalse) =>
        // we don't need this intermediate list and can remove it for performance,
        // but for the time being this is a good place to put a debugger
        val results = toIterable(data).iterator.map((initData, _)).map(evalLoop(condition)).toList
        val success = Intersect[Boolean].intersect(results)
        if (success) whenTrue(data)
        else whenFalse(data)
      case ExpAlg.FromOrigin(expression) =>
        evalLoop(expression)(initData, initData)
      case exp @ ExpAlg.EqualTo(value, whenTrue, whenFalse) =>
        if (exp.eq.eqv(value, data)) whenTrue(data) else whenFalse(data)
      case ExpAlg.Within(window, whenTrue, whenFalse) =>
        if (window.contains(data)) whenTrue(data) else whenFalse(data)
      case ExpAlg.Cond(condition, thenExpression, elseExpression) =>
        val success = evalLoop(condition)(input)
        if (success) evalLoop(thenExpression)(input)
        else evalLoop(elseExpression)(input)
      case ExpAlg.Collect(_, collector, expression, whenEmpty) =>
        collector(data).map((initData, _)).map(evalLoop(expression)).getOrElse(whenEmpty(data))
      case ExpAlg.And(combine, expressions) =>
        val results = expressions.map(evalLoop[T, B]).map(_(input))
        combine(results)
      case ExpAlg.Or(combine, expressions) =>
        combine(expressions.map(evalLoop[T, B]).map(_(input)))
    }
  }
}
