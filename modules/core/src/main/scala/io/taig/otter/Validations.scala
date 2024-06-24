package io.taig.otter

import java.util.regex.Pattern
import io.taig.otter.validation.Validation

trait Validations:
  val email: Validation[String, Constraint.Primitive[Nothing], String, Unit] = matches(Pattern.compile(".+@.+\\..+"))

  def matches(pattern: Pattern): Validation[String, Constraint.Primitive[Nothing], String, Unit] =
    Validation.when(Constraint.Primitive.Matches(pattern))(pattern.matcher(_).matches())
