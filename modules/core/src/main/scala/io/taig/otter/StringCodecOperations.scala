package io.taig.otter

import cats.syntax.all.*

import java.util.regex.Pattern

abstract class StringCodecOperations[+Self[_]: Codec, A]:
  protected def isEmpty(a: A): Boolean
  protected def empty: A

  def apply(minimum: Option[Int] = none, maximum: Option[Int] = none, matches: Option[Pattern] = none): Self[A]

  final def apply(minimum: Int, maximum: Int): Self[A] =
    apply(minimum = minimum.some, maximum = maximum.some, matches = none)
  final def matches(
      pattern: String,
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[A] = apply(minimum = none, maximum = none, matches = Pattern.compile(Pattern.quote(pattern)).some)
  final def required(maximum: Option[Int] = none, matches: Option[Pattern] = none): Self[A] =
    apply(minimum = 1.some, maximum, matches)
  final def required(maximum: Int, matches: Pattern): Self[A] =
    required(maximum = maximum.some, matches = matches.some)
  final def required(maximum: Int): Self[A] =
    required(maximum = maximum.some, matches = none)
  final def required(matches: Pattern): Self[A] =
    required(maximum = none, matches = matches.some)
  final val required: Self[A] = required()
  final val nonEmpty: Self[Option[A]] =
    apply(minimum = none, maximum = none, matches = none).imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))
