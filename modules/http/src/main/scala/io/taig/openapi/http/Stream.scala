package io.taig.openapi.http

import cats.Applicative
import cats.effect.std.Dispatcher
import cats.effect.{Async, Concurrent, IO, LiftIO}
import cats.syntax.all.*

import scala.reflect.ClassTag
import fs2.Stream as Fs2Stream

abstract class Stream[A]:
  type Effect[_]
  def isEmpty: Boolean
//  def imap[B](f: A => B)(g: B => A): TestStream[B]
  def toArray: Effect[Array[A]]

object Stream:
  def empty[A: ClassTag]: Stream[A] = new Stream[A]:
    override type Effect[a] = a
    override def isEmpty: Boolean = true
    override def toArray: Array[A] = Array.empty[A]

  def from[A](values: Array[A]): Stream[A] = new Stream[A]:
    override type Effect[a] = a
    override def isEmpty: Boolean = values.isEmpty
    override def toArray: Array[A] = values

  def from[F[_]: Concurrent, A: ClassTag](fs2: Fs2Stream[F, A]): Stream[A] = new Stream[A]:
    override type Effect[a] = F[a]
    override def isEmpty: Boolean = ???
    override def toArray: F[Array[A]] = fs2.compile.to(Array)
