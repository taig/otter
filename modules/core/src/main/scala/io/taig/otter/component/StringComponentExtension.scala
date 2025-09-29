package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.Undefined

import java.util.regex.Pattern

abstract class StringComponentExtension[+Self[_]: Invariant, A]:
  protected def isEmpty(a: A): Boolean
  protected def empty: A

  def apply(
      minimum: Undefined.Or[Int] = Undefined,
      maximum: Undefined.Or[Int] = Undefined,
      matches: Undefined.Or[Pattern] = Undefined
  ): Self[A]

  final def matches(
      pattern: String,
      minimum: Undefined.Or[Int] = Undefined,
      maximum: Undefined.Or[Int] = Undefined
  ): Self[A] = apply(minimum, maximum, matches = Pattern.compile(Pattern.quote(pattern)))

  final def required(
      maximum: Undefined.Or[Int] = Undefined,
      matches: Undefined.Or[Pattern] = Undefined
  ): Self[A] =
    apply(minimum = 1, maximum, matches)
  final val required: Self[A] = required()

  final val nonEmpty: Self[Option[A]] = apply().imap(_.some.filter(!isEmpty(_)))(_.getOrElse(empty))
