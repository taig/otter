package io.taig.otter

import cats.data.Chain

final case class Collection[+Of, A] private (asRead: Collection.Read[Of, A], asWrite: Collection.Write[Of, A])
    extends Schema[Of, A],
      Collection.Read[Of, A],
      Collection.Write[Of, A]:
  export asRead.schema
  override def imap[B](f: A => B)(g: B => A): Collection[Of, B] = Collection(asRead.map(f), asWrite.contramap(g))
  override def optional: Collection[Of, Option[A]] = Collection(asRead.optional, asWrite.optional)

object Collection:
  trait Operation[+Of]:
    def schema: Of

  sealed trait Read[+Of, +A] extends Schema.Read[Of, A], Collection.Operation[Of]:
    final override def map[B](f: A => B): Collection.Read[Of, B] = Read.Modify(this, f)
    override def optional: Collection.Read[Of, Option[A]] = Read.Optional(this)

  object Read:
    final case class Modify[Of, A, B](self: Collection.Read[Of, A], f: A => B) extends Collection.Read[Of, B]:
      export self.schema
    final case class Optional[Of, A](self: Collection.Read[Of, A]) extends Collection.Read[Of, Option[A]]:
      export self.schema
    final case class Root[S[_], A](schema: S[A]) extends Collection.Read[S[A], Chain[A]]

    def apply[S[_], A](schema: S[A]): Collection.Read[S[A], Chain[A]] = Root(schema)

  sealed trait Write[+Of, -A] extends Schema.Write[Of, A], Collection.Operation[Of]:
    final override def contramap[B](f: B => A): Collection.Write[Of, B] = Write.Modify(this, f)
    override def optional: Collection.Write[Of, Option[A]] = Write.Optional(this)

  object Write:
    final case class Modify[Of, A, B](self: Collection.Write[Of, A], f: B => A) extends Collection.Write[Of, B]:
      export self.schema
    final case class Optional[Of, A](self: Collection.Write[Of, A]) extends Collection.Write[Of, Option[A]]:
      export self.schema
    final case class Root[S[_], A](schema: S[A]) extends Collection.Write[S[A], Chain[A]]

    def apply[S[_], A](schema: S[A]): Collection.Write[S[A], Chain[A]] = Root(schema)

  def apply[S[_], A](schema: S[A]): Collection[S[A], Chain[A]] = Collection(Read(schema), Write(schema))
