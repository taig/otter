package io.taig.otter

opaque type Metadata = Map[String, Any]

object Metadata:
  opaque type Key[A] = String

  object Key:
    def apply[A](value: String): Metadata.Key[A] = value

  extension (self: Metadata)
    def apply[A](key: Metadata.Key[A]): Option[A] = self.get(key).map(_.asInstanceOf[A])
    def put[A](key: Metadata.Key[A], value: A): Metadata = self.updated(key, value)
    def remove[A](key: Metadata.Key[A]): Metadata = self.removed(key)

  val Empty: Metadata = Map.empty

  def one[A](key: Metadata.Key[A], value: A): Metadata = Map(key -> value)
