package io.taig.openapi.http

import cats.Id

sealed abstract class Entity:
  type Effect[_]
  def isEmpty: Boolean
  def consume: Effect[Array[Byte]]

object Entity:
  type Aux[F[_]] = Entity { type Effect[a] = F[a] }

  final case class Strict(consume: Array[Byte]) extends Entity:
    override type Effect[a] = Id[a]
    override def isEmpty: Boolean = consume.isEmpty

  val Empty: Entity = Strict(Array.emptyByteArray)
