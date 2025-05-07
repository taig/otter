package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.header.Accept
import io.taig.otter.Codec
import io.taig.otter.Invariant

sealed abstract class Bodies[+S[_], A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Bodies[S, B] = Bodies.Modify(self = this, f, g)

  final def orElse[T[_], B](bodies: Bodies[T, B]): Bodies[S + T, Either[A, B]] =
    Bodies.OrElse(left = this, right = bodies)

  final def or[T[_]](bodies: Bodies[T, A]): Bodies[S + T, A] = Bodies.Or(left = this, right = bodies)

object Bodies:
  final private[otter] case class Modify[S[_], A, B](self: Bodies[S, A], f: A => B, g: B => A) extends Bodies[S, B]

  final private[otter] case class Or[S[_], T[_], A, B](left: Bodies[S, A], right: Bodies[T, A]) extends Bodies[S + T, A]

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Bodies[S, A], right: Bodies[T, B])
      extends Bodies[S + T, Either[A, B]]

  private[otter] final case class Root[S[_], A](body: Body[S, A]) extends Bodies[S, A]

  given invariant[S[_]]: Invariant.Coproduct[Bodies[S, *], Bodies[S, *]] = new Invariant.Coproduct[Bodies[S, *], Bodies[S, *]]:
    override def result: Invariant[Bodies[S, *]] = this

    extension [A](self: Bodies[S, A])
      override def orElse[B](codec: Bodies[S, B]): Bodies[S, Either[A, B]] = self.orElse(codec)
      override def imap[B](f: A => B)(g: B => A): Bodies[S, B] = self.imap(f)(g)