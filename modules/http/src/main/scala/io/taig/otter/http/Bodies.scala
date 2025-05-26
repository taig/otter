package io.taig.otter.http

import cats.data.NonEmptyChain
import io.taig.otter.+
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.Enrichment
import io.taig.otter.operation.*

type Bodies[+S[_], A] = Enrichment[Bodies.Value[S, *], A]

object Bodies:
  sealed abstract class Value[+S[_], A] extends Product with Serializable:
    def toChain: NonEmptyChain[Body[S, ?]]

    final def satisfies(mediaRange: MediaRange): Boolean = ??? // toChain.exists(_.satisfies(mediaRange))

    final def matches(contentType: MediaType): Boolean = ??? // toChain.exists(_.matches(contentType))

    final def imap[B](f: A => B)(g: B => A): Value[S, B] = Value.Modify(self = this, f, g)

    final def orElse[T[_], B](bodies: Value[T, B]): Value[S + T, Either[A, B]] =
      Value.OrElse(left = this, right = bodies)

  object Value:
    final private[otter] case class Modify[S[_], A, B](self: Bodies.Value[S, A], f: A => B, g: B => A)
        extends Bodies.Value[S, B]:
      export self.toChain

    final private[otter] case class OrElse[S[_], T[_], A, B](left: Bodies.Value[S, A], right: Bodies.Value[T, B])
        extends Bodies.Value[S + T, Either[A, B]]:
      override def toChain: NonEmptyChain[Body[S + T, ?]] = left.toChain ++ right.toChain

    final private[otter] case class Root[S[_], A](body: Body[S, A]) extends Bodies.Value[S, A]:
      override def toChain: NonEmptyChain[Body[S, A]] = NonEmptyChain.one(body)

  given [S[_]]: SchemaInvariant[Bodies[S, *]] = new SchemaInvariant[Bodies[S, *]] {

    override def imap[A, B](fa: Bodies[S, A])(f: A => B)(g: B => A): Bodies[S, B] = ???

  }
