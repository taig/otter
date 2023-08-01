package io.taig.crock.schema

abstract class Printer[F[_], A]:
  def print[B](fb: F[B], b: B): A
