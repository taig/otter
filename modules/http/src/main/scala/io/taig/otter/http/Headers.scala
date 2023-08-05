package io.taig.otter.http

import cats.data.Chain

sealed abstract class Headers[A]:
  def toChain: Chain[Header[?]]

object Headers:
  private[otter] case object Root extends Headers[Unit]:
    override def toChain: Chain[Header[?]] = Chain.empty

  private[otter] case class One[A](header: Header[A]) extends Headers[A]:
    override def toChain: Chain[Header[A]] = Chain.one(header)

  val Empty: Headers[Unit] = Root
