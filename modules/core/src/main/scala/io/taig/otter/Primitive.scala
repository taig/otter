package io.taig.otter

sealed trait Primitive[A] extends Primitive.Reader[A], Primitive.Writer[A]

object Primitive:
  sealed trait Reader[+A]:
    def tpe: Type[?]

  object Reader:
    final case class Optional[A](self: Primitive.Reader[A]) extends Primitive.Reader[Option[A]]:
      export self.tpe

  sealed trait Writer[-A]:
    def tpe: Type[?]

  object Writer:
    final case class Contravariant[A, B](self: Primitive.Writer[A], f: B => A) extends Primitive.Writer[B]:
      export self.tpe

    final case class Optional[A](self: Primitive.Writer[A]) extends Primitive.Writer[Option[A]]:
      export self.tpe

  final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
    export self.tpe

  final case class Root[A](tpe: Type[A]) extends Primitive[A]
