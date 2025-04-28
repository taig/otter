package io.taig.otter

import cats.syntax.all.*

import java.util.regex.Pattern

abstract class StringCodecOperations[+Self[_]: Codec, A]:
  protected def isEmpty(a: A): Boolean
  protected def empty: A

  def apply(
      minimum: Value[Int] = Value.Default,
      maximum: Value[Int] = Value.Default,
      matches: Value[Pattern] = Value.Default
  ): Self[A]

  final def matches(
      pattern: String,
      minimum: Value[Int] = Value.Default,
      maximum: Value[Int] = Value.Default
  ): Self[A] = apply(minimum, maximum, matches = Pattern.compile(Pattern.quote(pattern)))

  final def required(maximum: Value[Int] = Value.Default, matches: Value[Pattern] = Value.Default): Self[A] =
    apply(minimum = 1, maximum, matches)
  final val required: Self[A] = required()

  final val nonEmpty: Self[Option[A]] = apply().imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))
