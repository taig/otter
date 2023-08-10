package io.taig.otter.http

import cats.data.Chain

sealed abstract class Headers[A]:
  def toChain: Chain[Header[?]]

object Headers:
  val Empty: Headers[Unit] = new Headers[Unit]:
    override def toChain: Chain[Header[?]] = Chain.empty

  def apply[A](header: Header[A]): Headers[A] = new Headers[A]:
    override def toChain: Chain[Header[A]] = Chain.one(header)
