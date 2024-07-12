package io.taig.otter

import io.taig.otter as Base

sealed abstract class ValidationWriter[+A]:
  def value: A

object ValidationWriter:
  sealed abstract class Value[+A] extends ValidationWriter[A]

  object Value:
    final case class Root[A](writer: Base.Value.Writer[Any, ?, A], value: A) extends ValidationWriter.Value[A]

  final case class Root[A, B](writer: Schema.Writer[Any, ?, A], value: A) extends ValidationWriter[A]

  def apply[A](writer: Schema.Writer[Any, ?, A], value: A): ValidationWriter[A] = Root(writer, value)
