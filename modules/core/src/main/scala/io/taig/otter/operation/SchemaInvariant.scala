package io.taig.otter.operation

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Convert
import io.taig.otter.Merge

import scala.annotation.targetName
import scala.compiletime.*
import io.taig.otter.Translator

trait SchemaInvariant[Self[_]] extends Invariant[Self]:
  self =>

  final protected given Invariant[Self] = this

  def enriched[A]: Enriched[Self[A]]

  extension [A](self: Self[A])
    // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
    // final def to[B](using convert: Convert[A, B]): F[B] = imap(convert.to)(convert.from)
    final inline def to[B]: Self[B] =
      val convert = summonInline[Convert[A, B]]
      self.imap(convert.to)(convert.from)

    def translate[F[_]](using translator: Translator[Self, F]): F[A] = translator.translate(self)

  extension [A, B](self: Self[(A, B)])
    final def merged(using merge: Merge[A, B]): Self[merge.Out] =
      self.imap(merge.apply)(merge.unapply)

  extension (self: Self[Unit])
    final def as[A](a: A): Self[A] = self.imap(_ => a)(_ => ())

    @targetName("asSingleton")
    final def as[A <: Singleton](a: A): Self[A] = self.imap(_ => a)(_ => ())

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): SchemaInvariant[T] = new SchemaInvariant[T]:
    override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object SchemaInvariant:
  inline def apply[Self[_]](using schema: SchemaInvariant[Self]): SchemaInvariant[Self] = schema

  trait Nullable[Self[_], +Nullable[_]](using self: NullableSchemaInvariant[Nullable, Self])
      extends SchemaInvariant[Self]:
    extension [A](sa: => Self[A])
      final def nullable: Nullable[Option[A]] = self(sa)
      final def nullable(default: A): Nullable[A] = self(sa, default)

  trait Recordable[Self[_], Record[_]](using self: RecordSchemaInvariant[Record, Self]):
    extension [A](self: => Self[A])
      def :*[B](schema: => Self[B])(using m: Merge[A, B]): Record[m.Out] = this.self.lift(self) :* schema
      def *:[B](schema: => Self[B])(using m: Merge[A, B]): Record[m.Out] = this.self.lift(self) :* schema
      def toRecord: Record[A] = this.self.lift(self)

  trait Unionable[Self[_], Union[_]](using self: UnionSchemaInvariant[Union, Self]):
    extension [A](self: => Self[A])
      def :+[B](schema: => Self[B]): Union[Either[A, B]] = this.self.lift(self) :+ schema
      inline def toUnion: Union[A] = this.self.lift(self)

    extension [A <: Matchable](self: Self[A])
      inline def |[B <: Matchable](schema: => Self[B]): Union[A | B] =
        self.toUnion.or(schema.toUnion)
