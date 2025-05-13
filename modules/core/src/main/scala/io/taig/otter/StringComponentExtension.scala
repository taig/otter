package io.taig.otter

import cats.syntax.all.*

import java.util.regex.Pattern

abstract class StringComponentExtension[+Self[_]: Invariant, A]:
  protected def isEmpty(a: A): Boolean
  protected def empty: A

  def apply(
      minimum: Argument[Int] = Argument.Default,
      maximum: Argument[Int] = Argument.Default,
      matches: Argument[Pattern] = Argument.Default
  ): Self[A]

  final def matches(
      pattern: String,
      minimum: Argument[Int] = Argument.Default,
      maximum: Argument[Int] = Argument.Default
  ): Self[A] = apply(minimum, maximum, matches = Pattern.compile(Pattern.quote(pattern)))

  final def required(
      maximum: Argument[Int] = Argument.Default,
      matches: Argument[Pattern] = Argument.Default
  ): Self[A] =
    apply(minimum = 1, maximum, matches)
  final val required: Self[A] = required()

  final val nonEmpty: Self[Option[A]] = apply().imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))
