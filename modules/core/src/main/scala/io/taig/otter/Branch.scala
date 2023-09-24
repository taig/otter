package io.taig.otter

import cats.syntax.all.*

final case class Branch[A, B](name: A, key: Schema.Value[A], value: Schema[B]):
  def isOptional: Boolean = value.isOptional

  def :+[C, D](branch: Branch[C, D]): Schema.Coproduct[B + D] = toCoproduct :+ branch
  def +:[C, D](branch: Branch[C, D]): Schema.Coproduct[D + B] = branch +: toCoproduct

  def toCoproduct: Schema.Coproduct[B] = Schema.Coproduct(this)
