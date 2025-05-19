package io.taig.otter
import scala.collection.immutable.SortedMap

final class Metadata(val toMap: SortedMap[String, Any]) extends AnyVal

object Metadata:
  opaque type Key[A] = String

  object Key:
    inline def apply[A](value: String): Metadata.Key[A] = value

  extension (self: Metadata)
    inline def contains[A](key: Metadata.Key[A]): Boolean = self.toMap.contains(key)
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    inline def get[A](key: Metadata.Key[A]): Option[A] = self.toMap.get(key).asInstanceOf[Option[A]]
    inline def put[A](key: Metadata.Key[A], value: A): Metadata = Metadata(self.toMap.updated(key, value))
    inline def remove[A](key: Metadata.Key[A]): Metadata = Metadata(self.toMap.removed(key))

  val Empty: Metadata = Metadata(SortedMap.empty)

  def one[A](key: Metadata.Key[A], value: A): Metadata = Metadata(SortedMap(key -> value))
