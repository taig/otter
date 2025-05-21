package io.taig.otter.http

import cats.Invariant
import cats.data.NonEmptyChain
import io.taig.otter.+
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType

sealed abstract class Bodies[+S[_], A] extends Product with Serializable:
  def toChain: NonEmptyChain[Body[S, ?]]

  final def satisfies(mediaRange: MediaRange): Boolean = toChain.exists(_.satisfies(mediaRange))

  final def matches(contentType: MediaType): Boolean = toChain.exists(_.matches(contentType))

  final def imap[B](f: A => B)(g: B => A): Bodies[S, B] = Bodies.Modify(self = this, f, g)

  final def orElse[T[_], B](bodies: Bodies[T, B]): Bodies[S + T, Either[A, B]] =
    Bodies.OrElse(left = this, right = bodies)

  final def or[T[_]](bodies: Bodies[T, A]): Bodies[S + T, A] = Bodies.Or(left = this, right = bodies)

object Bodies:
  final private[otter] case class Modify[S[_], A, B](self: Bodies[S, A], f: A => B, g: B => A) extends Bodies[S, B]:
    export self.toChain

  final private[otter] case class Or[S[_], T[_], A, B](left: Bodies[S, A], right: Bodies[T, A])
      extends Bodies[S + T, A]:
    override def toChain: NonEmptyChain[Body[S + T, ?]] = left.toChain ++ right.toChain

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Bodies[S, A], right: Bodies[T, B])
      extends Bodies[S + T, Either[A, B]]:
    override def toChain: NonEmptyChain[Body[S + T, ?]] = left.toChain ++ right.toChain

  final private[otter] case class Root[S[_], A](body: Body[S, A]) extends Bodies[S, A]:
    override def toChain: NonEmptyChain[Body[S, ?]] = NonEmptyChain.one(body)

  given invariant[S[_]]: Invariant[Bodies[S, *]] with
    override def imap[A, B](fa: Bodies[S, A])(f: A => B)(g: B => A): Bodies[S, B] =
      fa.imap(f)(g)
