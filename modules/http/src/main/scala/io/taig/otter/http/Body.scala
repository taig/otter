package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType

// TODO strict vs streaming (?)
sealed abstract class Body[+S[_], A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Body[S, B] = Body.Modify(self = this, f, g)

  final def orElse[T[_], B](body: Body[T, B]): Body[[a] =>> S[a] | T[a], Either[A, B]] =
    Body.OrElse(left = this, right = body)

  final def or[T[_]](body: Body[T, A]): Body[[a] =>> S[a] | T[a], A] = Body.Or(left = this, right = body)

object Body:
  private[otter] case object Empty extends Body[Nothing, Unit]

  final private[otter] case class Modify[S[_], A, B](self: Body[S, A], f: A => B, g: B => A) extends Body[S, B]

  final private[otter] case class Or[S[_], T[_], A, B](left: Body[S, A], right: Body[T, A])
      extends Body[[a] =>> S[a] | T[a], A]

  final private[otter] case class OrElse[S[_], T[_], A, B](left: Body[S, A], right: Body[T, B])
      extends Body[[a] =>> S[a] | T[a], Either[A, B]]

  final private[otter] case class Root[S[_], A](mediaType: MediaType, codec: Reference[S, A]) extends Body[S, A]
