package io.taig.otter

sealed abstract class Branch[+O <: Data, A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Branch[O, A]

  def imap[B](f: A => B)(g: B => A): Branch[O, B]

  def to[B](using convert: Convert[A, B]): Branch[O, B]

  final def :+[P <: Data, B](branch: Branch[P, B]): Union[O | P, Either[A, B]] = ???

  final def +:[P <: Data, B](branch: Branch[P, B]): Union[P | O, Either[B, A]] = ???

  def decode(data: Data): Codec.Result[Option[A]]

  def encode(a: A): O

object Branch:
  sealed abstract class Tagged[+O <: Data, A] extends Branch[O, A]:
    def discriminator: Discriminator

    override def modifyMetadata(f: Metadata => Metadata): Branch.Tagged[O, A] = ???

    override def imap[B](f: A => B)(g: B => A): Branch.Tagged[O, B] = ???

    override def to[B](using convert: Convert[A, B]): Branch.Tagged[O, B] = ???

  object Tagged:
    def apply[O <: Data, A](
        name: String,
        codec: Codec[Data.Primitive | O, A],
        discriminator: Discriminator
    ): Branch.Tagged[O, A] = ???

  sealed abstract class Untagged[O <: Data, A] extends Branch[O, A] {}

  // def nested[O <: Data, A](name: String, codec: Codec[O, A], discriminator: Discriminator.Nested): Branch[O, A] = new Branch[O, A](name, codec) {
  //   override def discriminator: Option[Discriminator] = discriminator.some
  //   override def metadata: Metadata = Metadata.Empty

  //   override def decode(data: Data): Result[A] = ???

  //   override def encode(a: A): O = ???

  // }

  // extension [O <: Data, A <: Matchable](self: Branch[O, A])
  //   inline def |[P <: Data, B <: Matchable](branch: Branch[P, B]): Branches[O | P, A | B] =
  //     self.toBranches | branch

  given [O <: Data, A]: Metadata.Ops[Branch[O, A]] with
    extension (self: Branch[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = self.modifyMetadata(f)
