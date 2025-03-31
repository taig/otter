package io.taig.otter

import cats.syntax.all.*

import java.util.regex.Pattern

abstract class StringCodecBuilder[A]:
  protected def isEmpty(a: A): Boolean
  protected def empty: A

  def apply(
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none,
      matches: Option[Pattern] = none
  ): Primitive[A]

  final def apply(minLength: Int, maxLength: Int): Primitive[A] =
    apply(minLength = minLength.some, maxLength = maxLength.some, matches = none)
  final def matches(
      pattern: String,
      minLength: Option[Int] = none,
      maxLength: Option[Int] = none
  ): Primitive[A] =
    apply(minLength = none, maxLength = none, matches = Pattern.compile(Pattern.quote(pattern)).some)
  final def required(maxLength: Option[Int] = none, matches: Option[Pattern] = none): Primitive[A] =
    apply(minLength = 1.some, maxLength, matches)
  final def required(maxLength: Int, matches: Pattern): Primitive[A] =
    required(maxLength = maxLength.some, matches = matches.some)
  final def required(maxLength: Int): Primitive[A] =
    required(maxLength = maxLength.some, matches = none)
  final def required(matches: Pattern): Primitive[A] =
    required(maxLength = none, matches = matches.some)
  final val required: Primitive[A] = required()
  final val nonEmpty: Primitive[Option[A]] =
    apply(minLength = none, maxLength = none, matches = none).imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))

object StringCodecBuilder:
  given [A]: Conversion[StringCodecBuilder[A], Primitive[A]] = _.apply()
