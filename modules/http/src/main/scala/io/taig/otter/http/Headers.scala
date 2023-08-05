package io.taig.otter.http

import cats.data.Chain

sealed abstract class Headers[A]:
  def toChain: Chain[Header[?]]

object Headers:
  val Empty: Headers[Unit] = new Headers:
    override def toChain: Chain[Header[?]] = Chain.empty
