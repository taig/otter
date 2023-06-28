package io.taig.openapi.http

abstract class Stream[+A]:
  def isEmpty: Boolean

object Stream:
  val Empty: Stream[Nothing] = new Stream[Nothing]:
    override def isEmpty: Boolean = true
