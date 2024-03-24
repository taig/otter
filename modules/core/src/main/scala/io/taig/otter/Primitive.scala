package io.taig.otter

sealed abstract class Primitive[+A] extends Schema[A]:
  self =>
  override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive[B]
  final override def optional: Primitive.Optional[Option[A]] = ??? // Primitive.Optional.Root(this)

  def tpe: Type[?]

object Primitive:
  sealed abstract class Required[+A] extends Primitive[A]:
    self =>
    final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive.Required[B] =
      Primitive.Required.Modify(this, f, g)

  object Required:
    final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

    final case class Modify[A, A1 >: A, B](primitive: Primitive.Required[A], f: A => B, g: B => A1)
        extends Primitive.Required[B]:
      export primitive.tpe

  sealed abstract class Optional[+A] extends Primitive[A]:
    self =>

    final override def imap[A1 >: A, B](f: A => B)(g: B => A1): Primitive.Optional[B] = ???

//   object Optional:
//     final case class Modify[A, B](primitive: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]

//     final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]
