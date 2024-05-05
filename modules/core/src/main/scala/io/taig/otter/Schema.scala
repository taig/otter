package io.taig.otter

import io.taig.otter.Collection.Read
import io.taig.otter.Schema.Write
import cats.data.Chain

sealed trait Schema[A] extends Schema.Read[A], Schema.Write[A]:
  def asRead: Schema.Read[A]
  def asWrite: Schema.Write[A]
  def imap[B](f: A => B)(g: B => A): Schema[B]

object Schema:
  sealed trait Read[+A]:
    def map[B](f: A => B): Schema.Read[B]

  sealed trait Write[-A]:
    def contramap[B](f: B => A): Schema.Write[B]

final case class Collection[+Of, A] private (asRead: Collection.Read[Of, A], asWrite: Collection.Write[Of, A])
    extends Schema[A],
      Collection.Read[Of, A],
      Collection.Write[Of, A]:
  export asRead.schema
  override def imap[B](f: A => B)(g: B => A): Collection[Of, B] = Collection(asRead.map(f), asWrite.contramap(g))

object Collection:
  trait Operation[+Of]:
    def schema: Of

  sealed trait Read[+Of, +A] extends Schema.Read[A], Collection.Operation[Of]:
    final override def map[B](f: A => B): Collection.Read[Of, B] = Read.Modify(this, f)

  object Read:
    final case class Modify[Of, +A, A1 >: A, B](self: Collection.Read[Of, A], f: A1 => B)
        extends Collection.Read[Of, B]:
      export self.schema
    final case class Optional[Of, A](self: Collection.Read[Of, A]) extends Collection.Read[Of, Option[A]]:
      export self.schema
    final case class Root[S[_], A](schema: S[A]) extends Collection.Read[S[A], Chain[A]]

    def apply[S[_], A](schema: S[A]): Collection.Read[S[A], Chain[A]] = Root(schema)

  sealed trait Write[+Of, -A] extends Schema.Write[A], Collection.Operation[Of]:
    final override def contramap[B](f: B => A): Collection.Write[Of, B] = Write.Modify(this, f)

  object Write:
    final case class Modify[Of, -A, A1 <: A, B](self: Collection.Write[Of, A], f: B => A1)
        extends Collection.Write[Of, B]:
      export self.schema
    final case class Optional[Of, A](self: Collection.Write[Of, A]) extends Collection.Write[Of, Option[A]]:
      export self.schema
    final case class Root[S[_], A](schema: S[A]) extends Collection.Write[S[A], Chain[A]]

    def apply[S[_], A](schema: S[A]): Collection.Write[S[A], Chain[A]] = Root(schema)

  def apply[S[_], A](schema: S[A]): Collection[S[A], Chain[A]] = Collection(Read(schema), Write(schema))
