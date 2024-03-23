package io.taig.otter

sealed abstract class Primitive[+M, A] extends Codec[M, A]:
  self =>
  override type Self[+m, a] <: Primitive[m, a]
  final override type Optional[+m, a] = Primitive.Optional[m, a]

  final override def optional: Primitive.Optional[M, Option[A]] = Primitive.Optional.Root(this)

object Primitive:
  sealed abstract class Required[+M, A] extends Primitive[M, A]:
    self =>
    final override type Self[+m, a] = Primitive.Required[m, a]

    final override def imap[B](f: A => B)(g: B => A): Primitive.Required[M, B] = Primitive.Required.Modify(this, f, g)
    final override def update[N](f: M => N): Primitive.Required[N, A] = Primitive.Required.Update(this, f)

  object Required:
    final case class Root[M, A](metadata: M, tpe: Type[A]) extends Primitive.Required[M, A]

    final case class Modify[M, A, B](primitive: Primitive.Required[M, A], f: A => B, g: B => A)
        extends Primitive.Required[M, B]:
      export primitive.metadata

    final case class Update[M, N, A](schema: Primitive.Required[M, A], f: M => N) extends Primitive.Required[N, A]:
      override def metadata: N = f(schema.metadata)

  sealed abstract class Optional[+M, A] extends Primitive[M, A]:
    self =>
    final override type Self[+m, a] = Primitive.Optional[m, a]

    final override def imap[B](f: A => B)(g: B => A): Primitive.Optional[M, B] = ???
    final override def update[N](f: M => N): Primitive.Optional[N, A] = Primitive.Optional.Update(this, f)

  object Optional:
    final case class Modify[M, A, B](primitive: Primitive[M, A], f: A => B, g: B => A) extends Primitive.Optional[M, B]:
      export primitive.metadata

    final case class Root[M, A](primitive: Primitive[M, A]) extends Primitive.Optional[M, Option[A]]:
      export primitive.metadata

    final case class Update[M, N, A](schema: Primitive.Optional[M, A], f: M => N) extends Primitive.Optional[N, A]:
      override def metadata: N = f(schema.metadata)
