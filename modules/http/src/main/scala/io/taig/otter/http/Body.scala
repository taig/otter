package io.taig.otter.http

import io.taig.otter.http.header.MediaType
import io.taig.otter.Reference

// TODO strict vs streaming (?)
sealed abstract class Body[+S, A] extends Product with Serializable:
  final def imap[B](f: A => B)(g: B => A): Body[S, B] = Body.Modify(self = this, f, g)

  final def orElse[T, B](body: Body[T, B]): Body[S | T, Either[A, B]] = Body.OrElse(left = this, right = body)

  final def or[T](body: Body[T, A]): Body[S | T, A] = Body.Or(left = this, right = body)

object Body:
  final private[otter] case class Modify[S, A, B](self: Body[S, A], f: A => B, g: B => A) extends Body[S, B]

  final private[otter] case class Or[S, T, A, B](left: Body[S, A], right: Body[T, A]) extends Body[S | T, A]

  final private[otter] case class OrElse[S, T, A, B](left: Body[S, A], right: Body[T, B])
      extends Body[S | T, Either[A, B]]

  final private[otter] case class Root[S[_], A](mediaType: MediaType, codec: Reference[S, A]) extends Body[S[A], A]
