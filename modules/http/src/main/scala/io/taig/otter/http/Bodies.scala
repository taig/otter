package io.taig.otter.http

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.operation.*

final case class Bodies[+S[_], A](self: Enrichment[Bodies.Value[S, A]]):
  inline def value: Bodies.Value[S, A] = self.self

  def toChain: NonEmptyChain[Body[S, ?]] = self.self.toChain
  def satisfies(mediaRange: MediaRange): Boolean = self.self.satisfies(mediaRange)
  def matches(contentType: MediaType): Boolean = self.self.matches(contentType)

  def orElse[T[_], B](bodies: Bodies[T, B]): Bodies[S + T, Either[A, B]] =
    Bodies(Enrichment(value.orElse(bodies.value)))

  def or[T[_]](bodies: Bodies[T, A]): Bodies[S + T, A] =
    Bodies(Enrichment(value.or(bodies.value)))

  def :+[T[_], B](body: Body[T, B]): Bodies[S + T, Either[A, B]] = orElse(body.toBodies)

  def +:[T[_], B](body: Body[T, B]): Bodies[S + T, Either[B, A]] = body.toBodies.orElse(this)

object Bodies:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def toChain: NonEmptyChain[Body[S, ?]]

    final def satisfies(mediaRange: MediaRange): Boolean = toChain.exists(_.satisfies(mediaRange))

    final def matches(contentType: MediaType): Boolean = toChain.exists(_.matches(contentType))

    final def imap[B](f: A => B)(g: B => A): Bodies.Value[S, B] = Value.Modify(self = this, f, g)

    final def orElse[T[_], B](bodies: Bodies.Value[T, B]): Bodies.Value[S + T, Either[A, B]] =
      Value.OrElse(left = this, right = bodies)

    final def or[T[_]](bodies: Bodies.Value[T, A]): Bodies.Value[S + T, A] =
      Value.Or(left = this, right = bodies)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Bodies.Value[S, A], f: A => B, g: B => A)
        extends Bodies.Value[S, B]:
      export self.toChain

    final private[otter] case class Or[S[_], T[_], A](left: Bodies.Value[S, A], right: Bodies.Value[T, A])
        extends Bodies.Value[S + T, A]:
      override def toChain: NonEmptyChain[Body[S + T, ?]] = left.toChain ++ right.toChain

    final private[otter] case class OrElse[S[_], T[_], A, B](left: Bodies.Value[S, A], right: Bodies.Value[T, B])
        extends Bodies.Value[S + T, Either[A, B]]:
      override def toChain: NonEmptyChain[Body[S + T, ?]] = left.toChain ++ right.toChain

    final private[otter] case class Root[S[_], A](body: Body[S, A]) extends Bodies.Value[S, A]:
      override def toChain: NonEmptyChain[Body[S, A]] = NonEmptyChain.one(body)

  extension [S[_], A <: Matchable](self: Bodies[S, A])
    inline def |[T[_], B <: Matchable](body: Body[T, B]): Bodies[S + T, A | B] = (self :+ body).imap {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

  given [S[_]]: EnrichedSchemaInvariant[Bodies[S, *]] with
    override def imap[A, B](fa: Bodies[S, A])(f: A => B)(g: B => A): Bodies[S, B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Bodies[S, A])
      override def metadata: Metadata = self.self.metadata
      override def metadata(f: Metadata => Metadata): Bodies[S, A] =
        self.copy(self = self.self.modifyMetadata(f))
