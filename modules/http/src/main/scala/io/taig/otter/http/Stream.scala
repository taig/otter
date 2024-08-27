package io.taig.otter.http

abstract class Stream[F[_], A]:
  def map[B](f: A => B): Stream[F, B]
