package com.rallyhealth.vapors.core.algebra

import cats.Eq
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.data.{NamedLens, Window}

sealed trait ExpAlg[O, T, A]

object ExpAlg {

  final case class Pure[O, T, A](
    label: String,
    always: T => A,
  ) extends ExpAlg[O, T, A]

  final case class FromOrigin[O, T, A](expression: FreeApplicative[ExpAlg[O, O, *], A]) extends ExpAlg[O, T, A]

  final case class Select[O, T, U, A](
    selector: NamedLens[T, U],
    expression: FreeApplicative[ExpAlg[O, U, *], A],
  ) extends ExpAlg[O, T, A]

  final case class ForAll[O, T, U, A](
    toIterable: T => IterableOnce[U],
    condition: FreeApplicative[ExpAlg[O, U, *], Boolean],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[O, T, A]

  final case class Exists[O, T, U, A](
    toIterable: T => IterableOnce[U],
    condition: FreeApplicative[ExpAlg[O, U, *], Boolean],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[O, T, A]

  final case class EqualTo[O, T : Eq, A](
    value: T,
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[O, T, A] {
    def eq: Eq[T] = Eq[T]
  }

  final case class Within[O, T, A](
    window: Window[T],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[O, T, A]

  // TODO: Use partial function?
  // TODO: Less generic name for query language
  final case class Collect[O, T, U, A](
    subtypeName: String,
    collect: T => Option[U],
    expression: FreeApplicative[ExpAlg[O, U, *], A],
    whenEmpty: T => A,
  ) extends ExpAlg[O, T, A]

  final case class Cond[O, T, A](
    condition: FreeApplicative[ExpAlg[O, T, *], Boolean],
    thenExpression: FreeApplicative[ExpAlg[O, T, *], A],
    elseExpression: FreeApplicative[ExpAlg[O, T, *], A],
  ) extends ExpAlg[O, T, A]

  final case class And[O, T, A](
    combine: List[A] => A,
    expressions: List[FreeApplicative[ExpAlg[O, T, *], A]],
  ) extends ExpAlg[O, T, A]

  final case class Or[O, T, A](
    combine: List[A] => A,
    expressions: List[FreeApplicative[ExpAlg[O, T, *], A]],
  ) extends ExpAlg[O, T, A]
}
