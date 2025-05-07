package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType
import cats.data.Chain
import io.taig.otter.http.header.MediaRange
import io.taig.otter.Invariant

// TODO strict vs streaming (?)
sealed abstract class Body[+S[_], A] extends Product with Serializable:
  def satisfies(mediaRange: MediaRange): Boolean

  final def orElse[T[_], B](body: Body[T, B]): Bodies[S + T, Either[A, B]] = toBodies.orElse(body.toBodies)
  
  final def or[T[_]](body: Body[T, A]): Bodies[S + T, A] = toBodies.or(body.toBodies)

  final def imap[B](f: A => B)(g: B => A): Body[S, B] = Body.Modify(self = this, f, g)

  final def toBodies: Bodies[S, A] = Bodies.Root(body = this)

object Body:
  final private[otter] case class Modify[S[_], A, B](self: Body[S, A], f: A => B, g: B => A) extends Body[S, B]:
    export self.satisfies

  final private[otter] case class Root[S[_], A](mediaType: MediaType, codec: Reference[S, A]) extends Body[S, A]:
    override def satisfies(mediaRange: MediaRange): Boolean = mediaType.satisfies(mediaRange)

  given [S[_]]: Invariant.Coproduct[Body[S, *], Bodies[S, *]] with
      override def result: Invariant[Bodies[S, *]] = Bodies.invariant
      
      extension [A](self: Body[S, A])
        override def imap[B](f: A => B)(g: B => A): Body[S, B] = self.imap(f)(g)
        override def orElse[B](codec: Body[S, B]): Bodies[S, Either[A, B]] = self.orElse(codec)
