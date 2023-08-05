package io.taig.otter.http

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
