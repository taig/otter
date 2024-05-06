package io.taig.otter

sealed trait Primitive[A] extends Schema[Nothing, A], Primitive.Read[A], Primitive.Write[A]:
  override def imap[B](f: A => B)(g: B => A): Primitive[B] = ???
  override def optional: Primitive[Option[A]] = ???

object Primitive:
  trait Operation:
    def tpe: Type[?]

  sealed trait Required[A] extends Primitive[A], Primitive.Required.Read[A], Primitive.Required.Write[A]:
    override def imap[B](f: A => B)(g: B => A): Primitive.Required[B] = ???
    override def optional: Primitive[Option[A]] = ???

  object Required:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def optional: Primitive.Read[Option[A]] = ???

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def optional: Primitive.Write[Option[A]] = ???

  sealed trait Read[+A] extends Schema.Read[Nothing, A]:
    override def map[B](f: A => B): Primitive.Read[B] = ???
    override def optional: Primitive.Read[Option[A]] = ???

  sealed trait Write[-A] extends Schema.Write[Nothing, A]:
    override def contramap[B](f: B => A): Primitive.Write[B] = ???
    override def optional: Primitive.Write[Option[A]] = ???
