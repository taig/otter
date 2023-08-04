package io.taig.otter.http

abstract class Stream:
  def isEmpty: Boolean

object Stream:
  val Empty: Stream = new Stream:
    override def isEmpty: Boolean = true
