package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.Undefined
import io.taig.data.Encoder
import io.taig.otter.operation.StringOperation
import io.taig.validation
import io.taig.validation.Constraint
import io.taig.validation.Validation
import io.taig.validation.cistring.given
import io.taig.validation.std
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveComponent[+Self[_]: Invariant](using operation: StringOperation[Self]):
  self =>

  private given Encoder[CIString] = Encoder[String].contramap(_.toString)

  def cistring(validation: Validation[Constraint.Primitive.Text, CIString]): Self[CIString] =
    operation.string(validation = validation.contramap(CIString.apply)).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring(validation = Validation.valid)

  def cistring(
      minimum: Undefined.Or[Int] = Undefined,
      maximum: Undefined.Or[Int] = Undefined,
      pattern: Undefined.Or[Pattern] = Undefined
  ): Self[CIString] =
    val validation = (
      minimum.map(minimum => std.text.minimum[CIString](reference = minimum.toLong)).toList ++
        maximum.map(maximum => std.text.maximum[CIString](reference = maximum.toLong)).toList ++
        pattern.map(std.text.matches[CIString]).toList
    ).foldLeft[Validation[Constraint.Primitive.Text, CIString]](Validation.valid)(_ & _)

    self.cistring(validation)

  extension (x: cistring.type)
    def required(
        maximum: Undefined.Or[Int] = Undefined,
        pattern: Undefined.Or[Pattern] = Undefined
    ): Self[CIString] = self.cistring(minimum = 1, maximum, pattern)

    def required: Self[CIString] = required()

    def nonEmpty: Self[Option[CIString]] = self.cistring.imap(_.some.filter(_.nonEmpty))(_.getOrElse(CIString.empty))
