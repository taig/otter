package io.taig.otter

import cats.Order
import cats.Show
import cats.implicits.*

import scala.collection.immutable.SortedMap

opaque type Metadata = SortedMap[Metadata.Key[?], Any]

object Metadata:
  final case class Key[+A](namespace: String, identifier: String)

  object Key:
    object Namespace:
      val Global: String = "*"

    def apply[A](identifier: String): Metadata.Key[A] =
      Key(namespace = Namespace.Global, identifier)

    given [A] => Order[Metadata.Key[A]] = Order.by(key => (key.namespace, key.identifier))

  extension (self: Metadata)
    inline def toSortedMap: SortedMap[Metadata.Key[?], Any] = self

    def contains[A](key: Metadata.Key[A]): Boolean = toSortedMap.contains(key)

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def get[A](key: Metadata.Key[A]): Option[A] = toSortedMap
      .get(key)
      .flatMap(value => Either.catchOnly[ClassCastException](value.asInstanceOf[A]).toOption)

    def put[A](key: Metadata.Key[A], value: A): Metadata = toSortedMap.updated(key, value)

    def remove[A](key: Metadata.Key[A]): Metadata = toSortedMap.removed(key)

    def ++(metadata: Metadata): Metadata = toSortedMap ++ metadata.toSortedMap

  val Empty: Metadata = SortedMap.empty

  def one[A](key: Metadata.Key[A], value: A): Metadata = SortedMap(key -> value)

  given Show[Metadata] = _.toSortedMap.map((key, value) => s"$key=$value").mkString("[", ",", "]")
