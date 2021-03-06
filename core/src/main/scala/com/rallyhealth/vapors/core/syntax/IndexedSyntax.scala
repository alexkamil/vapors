package com.rallyhealth.vapors.core.syntax

import cats.Semigroup
import cats.data.NonEmptySet
import com.rallyhealth.vapors.core.data.Indexed

trait IndexedSyntax {

  implicit def indexedOps[C, K, V](collection: C): IndexedOps[C] = new IndexedOps(collection)
}

final class IndexedOps[C](private val collection: C) extends AnyVal {

  def get[K, V](key: K)(implicit indexedM: Indexed[C, K, V]): V = indexedM.get(collection)(key)

  def filterKeys[K, V](
    keys: NonEmptySet[K],
  )(implicit
    indexedM: Indexed[C, K, V],
    semigroupM: Semigroup[V],
  ): V = keys.toNonEmptyList.map(get(_)).reduce
}
