package io.taig.openapi.http

import cats.Applicative
import cats.syntax.all.*

abstract class Entity:
  def isEmpty: Boolean
  def consume[F[_]: Applicative]: F[Array[Byte]]

object Entity:
  def strict(body: Array[Byte]): Entity = new Entity:
    override def isEmpty: Boolean = body.isEmpty
    override def consume[F[_]: Applicative]: F[Array[Byte]] = body.pure[F]

  val Empty: Entity = strict(Array.emptyByteArray)
