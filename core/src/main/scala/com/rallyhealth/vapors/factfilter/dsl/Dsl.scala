package com.rallyhealth.vapors.factfilter.dsl

import cats.Eq
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.factfilter.data._

private[dsl] class Dsl extends TypedFactOps {

  def forall[T, V](cond: CondExp[V])(implicit ev: T <:< IterableOnce[V]): CondExp[T] = liftCondExp {
    ExpAlg.ForAll[Facts, T, V, Boolean](ev, cond, True, False)
  }

  def exists[T, V](cond: CondExp[V])(implicit ev: T <:< IterableOnce[V]): CondExp[T] = liftCondExp {
    ExpAlg.Exists[Facts, T, V, Boolean](ev, cond, True, False)
  }

  def lessThan[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[Facts, T, Boolean](Window.lessThan(upperBound), True, False)
  }

  def lessThanOrEqual[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[Facts, T, Boolean](Window.lessThanOrEqual(upperBound), True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[Facts, T, Boolean](Window.greaterThan(lowerBound), True, False)
  }

  def greaterThanOrEqual[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[Facts, T, Boolean](Window.greaterThanOrEqual(lowerBound), True, False)
  }

  def equalTo[T : Eq](value: T): CondExp[T] = liftCondExp {
    ExpAlg.EqualTo[Facts, T, Boolean](value, True, False)
  }

  // TODO: Figure out how to use this conditional tertiary expression

  def when[T, A](exp: Exp[T, Boolean])(thenExp: Exp[T, A])(elseExp: Exp[T, A]): Exp[T, A] = liftExp {
    ExpAlg.Cond[Facts, T, A](exp, thenExp, elseExp)
  }

  def and[T, A : Intersect](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.And[Facts, T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.Or[Facts, T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  // TODO: Make this generic over output type that can be converted to boolean?
  implicit def isTruthy[T](exp: TerminalFactsExp): CondExp[T] = liftCondExp {
    ExpAlg.FromOrigin[Facts, T, Boolean](exp.map(_.isTrue))
  }

  def alwaysTrue[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("True", _ => true))

  def alwaysFalse[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("False", _ => false))

  def alwaysMatch: TerminalFactsExp = liftTermExp(ExpAlg.Pure("AlwaysMatch", FactsMatch(_)))

  def alwaysEmpty: TerminalFactsExp = liftTermExp(ExpAlg.Pure("AlwaysEmpty", _ => NoFactsMatch()))

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  /** @see [[FactsExp]] */
  private def liftTermExp(exp: ExpAlg[Facts, Facts, ResultSet]): TerminalFactsExp =
    FreeApplicative.lift(exp)

  /** @see [[CondExp]] */
  private def liftCondExp[X](exp: ExpAlg[Facts, X, Boolean]): CondExp[X] = FreeApplicative.lift(exp)

  /** @see [[Exp]] */
  private def liftExp[X, A](exp: ExpAlg[Facts, X, A]): Exp[X, A] = FreeApplicative.lift(exp)
}
