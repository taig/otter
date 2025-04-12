package io.taig.otter
import scala.collection.immutable.SortedMap

opaque type Metadata = SortedMap[String, Any]

object Metadata:
  opaque type Key[A] = String

  object Key:
    inline def apply[A](value: String): Metadata.Key[A] = value

  extension (self: Metadata)
    inline def toMap: SortedMap[String, Any] = self
    inline def contains[A](key: Metadata.Key[A]): Boolean = toMap.contains(key)
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    inline def get[A](key: Metadata.Key[A]): Option[A] = self.get(key).asInstanceOf[Option[A]]
    inline def put[A](key: Metadata.Key[A], value: A): Metadata = self.updated(key, value)
    inline def remove[A](key: Metadata.Key[A]): Metadata = self.removed(key)

  val Empty: Metadata = SortedMap.empty

  def one[A](key: Metadata.Key[A], value: A): Metadata = SortedMap(key -> value)
