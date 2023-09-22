package io.taig.otter.schema

import cats.data.{Ior, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violation

final case class Branch[A, B](name: A, key: Schema.Value[A], value: Schema[B]):
  def isOptional: Boolean = value.isOptional

  def :+[C, D](branch: Branch[C, D]): Coproduct[B + D] = ??? // toCoproduct :+ branch
  def +:[C, D](branch: Branch[C, D]): Coproduct[D + B] = ??? // branch +: toCoproduct
