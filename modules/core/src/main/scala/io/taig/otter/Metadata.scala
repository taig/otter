package io.taig.otter

import cats.Order
import cats.Show
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

opaque type Metadata = SortedMap[(Metadata.Namespace, Metadata.Key[Any]), Any]

object Metadata:
  opaque type Key[+A] = String

  object Key:
    def apply[A](identifier: String): Metadata.Key[A] = identifier

    given [A] => (order: Order[String]) => Order[Metadata.Key[A]] = order

  opaque type Namespace = String

  object Namespace:
    val Global: Metadata.Namespace = "*"

    def apply(identifier: String): Metadata.Namespace = identifier

  extension (self: Metadata)
    inline def toSortedMap: SortedMap[(Metadata.Namespace, Metadata.Key[Any]), Any] = self

    def contains[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Boolean =
      toSortedMap.contains((namespace, key))

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def get[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Option[A] = toSortedMap
      .get((namespace, key))
      .flatMap(value => Either.catchOnly[ClassCastException](value.asInstanceOf[A]).toOption)

    def put[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): Metadata =
      toSortedMap.updated((namespace, key), value)

    def remove[A](namespace: Metadata.Namespace, key: Metadata.Key[A]): Metadata =
      toSortedMap.removed((namespace, key))

    def ++(metadata: Metadata): Metadata = toSortedMap ++ metadata.toSortedMap

  val Empty: Metadata = SortedMap.empty

  def one[A](namespace: Metadata.Namespace, key: Metadata.Key[A], value: A): Metadata =
    SortedMap((namespace, key) -> value)

  given Show[Metadata] = _.toSortedMap.map((key, value) => s"$key=$value").mkString("[", ",", "]")
