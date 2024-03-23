package io.taig.otter

sealed abstract class Primitive[+A, B] extends Schema[A, B]:
  override type Self[+a, b] <: Primitive[a, b]
  final override type Optional[+a, b] = Primitive.Optional[a, b]

  final override def optional: Primitive.Optional[A, Option[B]] = ??? // Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+A, B] extends Primitive[A, B]:
    final override type Self[+a, b] = Primitive.Required[a, b]

    final override def imap[C](f: B => C)(g: C => B): Primitive.Required[A, C] = ???
    final override def update[C](f: A => C): Required[C, B] = ???

  object Required:
    final case class Root[A, B](metadata: A, tpe: Type[B]) extends Primitive.Required[A, B]

    final case class Modify[A, B, C](schema: Primitive.Required[A, B], f: B => C, g: C => B)
        extends Primitive.Required[A, C]:
      export schema.metadata

    final case class Update[A, B, C](schema: Primitive.Required[A, B], f: A => C) extends Primitive.Required[C, B]:
      override def metadata: C = f(schema.metadata)

  sealed abstract class Optional[+A, B] extends Primitive[A, B]:
    final override type Self[+a, b] = Primitive.Optional[a, b]

    final override def imap[C](f: B => C)(g: C => B): Primitive.Optional[A, C] = ???
    override def update[C](f: A => C): Primitive.Optional[C, B] = ???

  object Optional:
    final case class Root[A, B](metadata: A, schema: Primitive[A, B]) extends Primitive.Optional[A, Option[B]]

    final case class Update[A, B, C](schema: Primitive.Optional[A, B], f: A => C) extends Primitive.Optional[C, B]:
      override def metadata: C = f(schema.metadata)
