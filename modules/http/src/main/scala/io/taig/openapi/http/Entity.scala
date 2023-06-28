package io.taig.openapi.http

import cats.Applicative
import cats.syntax.all.*

sealed abstract class Entity[A]:
  type Effect[a]
  def isEmpty: Boolean
  def isStreaming: Boolean
  def consume: Effect[Array[A]]

object Entity:
  type Aux[F[_], A] = Entity[A] { type Effect[a] = F[a] }

  final case class Strict[F[_]: Applicative, A](values: Array[A]) extends Entity[A]:
    override type Effect[a] = F[a]
    override def isEmpty: Boolean = values.isEmpty
    override def isStreaming: Boolean = false
    override def consume: F[Array[A]] = values.pure[F]

  abstract class Streaming[F[_], A] extends Entity[A]:
    override type Effect[a] = F[a]
    override def isStreaming: Boolean = true
