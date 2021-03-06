package com.rallyhealth.vapors.factfilter

import cats.Order
import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data.NamedLens

package object data {

  /**
    * Alias for a collection of facts with no restriction on Scala type.
    *
    * @note This is the input for a top-level fact filter expression.
    */
  final type Facts = NonEmptyList[Fact]

  /**
    * An ordered set of untyped [[Fact]]s.
    */
  final type FactSet = Set[Fact]

  /**
    * An ordered set of [[TypedFact]]s.
    *
    * @note not to be confused with a [[FactTypeSet]] (which is a set of [[FactType]]s, with not values)
    */
  final type TypedFactSet[T] = Set[TypedFact[T]]

  /**
    * A [[NamedLens]] defined over a [[TypedFact]] of a known type.
    */
  final type FactLens[T, V] = NamedLens[TypedFact[T], V]

  /**
    * A useful alias for building a [[NamedLens]] by passing the identity lens as a starting point to a function.
    */
  final type FactLensId[T] = NamedLens.Id[TypedFact[T]]

  /**
    * Alias for a collection of facts of a specific type.
    */
  final type FactsOfType[T] = NonEmptyList[TypedFact[T]]

  final type TypedFacts[T] = List[TypedFact[T]]

  final type OrderTypedFacts[T] = Order[TypedFact[T]]
}
