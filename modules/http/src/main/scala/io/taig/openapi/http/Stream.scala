package io.taig.openapi.http

abstract class Stream[A]:
  def isEmpty: Boolean
