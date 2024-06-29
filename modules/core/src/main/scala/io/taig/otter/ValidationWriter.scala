package io.taig.otter

import cats.Id as Identity

sealed abstract class ValidationWriter[+A]:
  def value: A

object ValidationWriter:
  final case class Root[A](writer: Schema.Writer[Identity, ?, A], value: A) extends ValidationWriter[A]

  def apply[A](writer: Schema.Writer[Identity, ?, A], value: A): ValidationWriter[A] = Root(writer, value)
