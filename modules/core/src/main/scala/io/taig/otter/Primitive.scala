package io.taig.otter

sealed abstract class Primitive[M, A] extends Schema[M, A]:
  self =>
  override type Self[a] <: Primitive[M, a]
  final override type Optional[a] = Primitive.Optional[M, a]

//   final override def optional: Primitive.Optional[Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[M, A] extends Primitive[M, A]:
    self =>
    final override type Self[a] = Primitive.Required[M, a]

//     final override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = Primitive.Required.Modify(this, f, g)

//   object Required:
//     final case class Root[A](tpe: Type[A]) extends Primitive.Required[A]

//     final case class Modify[A, B](primitive: Primitive.Required[A], f: A => B, g: B => A) extends Primitive.Required[B]

  sealed abstract class Optional[M, A] extends Primitive[M, A]:
    self =>
    final override type Self[a] = Primitive.Optional[M, a]

//     final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[B] = ???

//   object Optional:
//     final case class Modify[A, B](primitive: Primitive[A], f: A => B, g: B => A) extends Primitive.Optional[B]

//     final case class Root[A](primitive: Primitive[A]) extends Primitive.Optional[Option[A]]
