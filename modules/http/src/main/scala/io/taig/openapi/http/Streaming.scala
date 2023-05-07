package io.taig.openapi.http

import cats.Applicative
import cats.effect.{Concurrent, IO, LiftIO}
import cats.syntax.all.*

import scala.reflect.ClassTag
import fs2.Stream

abstract class Streaming[A]:
//  def imap[B](f: A => B)(g: B => A): TestStream[B]
  def toArray[F[_]: Concurrent: LiftIO]: F[Array[A]]

object Streaming:
  def empty[A: ClassTag]: Streaming[A] = ???

  def from[A](values: Array[A]): Streaming[A] = ???

  def from[A: ClassTag](fs2: Stream[IO, A]): Streaming[A] = new Streaming[A]:
    override def toArray[F[_]: Concurrent: LiftIO]: F[Array[A]] = fs2.compile.to(Array).to[F]
