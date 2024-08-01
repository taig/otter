package io.taig.otter

opaque type Metadata = Map[String, Any]

object Metadata:
  opaque type Key[A] = String

  object Key:
    def apply[A](value: String): Metadata.Key[A] = value

  extension (self: Metadata)
    def get[A](key: Metadata.Key[A]): Option[A] = self.get(key).map(_.asInstanceOf[A])
    def put[A](key: Metadata.Key[A], value: A): Metadata = self.updated(key, value)
    def remove[A](key: Metadata.Key[A]): Metadata = self.removed(key)

  trait Ops[A]:
    extension (self: A)
      def metadata: Metadata
      def modifyMetadata(f: Metadata => Metadata): A
      def apply[B](key: Metadata.Key[B]): Option[B] = metadata.get[B](key)
      def apply[B](key: Metadata.Key[B], value: Option[B]): A =
        modifyMetadata(metadata => value.fold(metadata.remove(key))(metadata.put(key, _)))
      def apply[B](key: Metadata.Key[B], value: B): A = modifyMetadata(_.put(key, value))

  val Empty: Metadata = Map.empty

  def one[A](key: Metadata.Key[A], value: A): Metadata = Map(key -> value)
