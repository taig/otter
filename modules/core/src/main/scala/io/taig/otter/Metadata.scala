package io.taig.otter

import cats.Show
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

final class Metadata(val toMap: SortedMap[String, Any]) extends AnyVal:
  override def toString: String = toMap.map { (key, value) => s"$key=$value" }.mkString("[", ",", "]")

object Metadata:
  opaque type Key[A] = String

  object Key:
    inline def apply[A](name: String): Metadata.Key[A] = name

  extension (self: Metadata)
    inline def contains[A](key: Metadata.Key[A]): Boolean = self.toMap.contains(key)
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    inline def get[A](key: Metadata.Key[A]): Option[A] = self.toMap
      .get(key)
      .flatMap(value => Either.catchOnly[ClassCastException](value.asInstanceOf[A]).toOption)
    inline def put[A](key: Metadata.Key[A], value: A): Metadata = Metadata(self.toMap.updated(key, value))
    inline def remove[A](key: Metadata.Key[A]): Metadata = Metadata(self.toMap.removed(key))

  val Empty: Metadata = Metadata(SortedMap.empty)

  def one[A](key: Metadata.Key[A], value: A): Metadata = Metadata(SortedMap(key -> value))

  given Show[Metadata] = Show.fromToString
