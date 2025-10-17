package io.taig.otter

import io.taig.validation
import io.taig.validation.Validation
import io.github.iltotore.iron.{Constraint as _, *}
import io.github.iltotore.iron.constraint.all.*
import scala.compiletime.*

trait ValidationOf[S, A, B]:
  def apply(): Validation[S, A]

object ValidationOf:
  inline given [V <: Int]: ValidationOf[Constraint.Primitive.Text, String, MinLength[V]] = 
    new ValidationOf[Constraint.Primitive.Text, String, MinLength[V]]:
      override def apply(): Validation[Constraint.Primitive.Text, String] =
        validation.std.string.minimum(reference = erasedValue[V])

  inline given [V <: Int]: ValidationOf[Constraint.Primitive.Text, String, MaxLength[V]] =
    new ValidationOf[Constraint.Primitive.Text, String, MaxLength[V]]:
      override def apply(): Validation[Constraint.Primitive.Text, String] =
        validation.std.string.maximum(reference = erasedValue[V])

  inline given [S, A, B](using mirror: IntersectionTypeMirror[B]): ValidationOf[S, A, B] = 
    new ValidationOf[S, A, B]:
      override def apply(): Validation[S, A] = ???


object Playground:
  @main
  def run: Unit = {
    val validation = summon[ValidationOf[Constraint.Primitive.Text, String, MinLength[5] & MaxLength[10]]].apply()
    println(validation.validate("hi"))
  }