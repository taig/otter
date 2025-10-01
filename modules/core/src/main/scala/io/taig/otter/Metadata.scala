package io.taig.otter

import cats.Show
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

opaque type Metadata = SortedMap[String, Any]

object Metadata:
  opaque type Key[A] = String

  object Key:
    inline def apply[A](name: String): Metadata.Key[A] = name

  extension (self: Metadata)
    inline def toSortedMap: SortedMap[String, Any] = self

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
