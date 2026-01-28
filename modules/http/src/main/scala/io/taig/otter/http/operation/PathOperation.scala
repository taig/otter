package io.taig.otter.http.operation

import io.taig.otter.InvariantK
import io.taig.otter.http.Http

trait PathOperation[F[_]]:
  def lift[A](segment: Http.Segment[A]): F[A]

object PathOperation:
  trait Read[F[_]] extends PathOperation[F]:
    def lift[A](segment: Http.Segment.Read[A]): F[A]

    final override def lift[A](segment: Http.Segment[A]): F[A] = lift(segment: Http.Segment.Read[A])

  trait Write[F[_]] extends PathOperation[F]:
    def lift[A](segment: Http.Segment.Write[A]): F[A]

  given InvariantK[PathOperation] = ???
