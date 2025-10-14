package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.Undefined
import io.taig.otter.operation.StringOperation
import io.taig.validation
import io.taig.validation.Constraint
import io.taig.validation.Validation
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveComponent[+Self[_]: Invariant](using operation: StringOperation[Self]):
  self =>

  def cistring(validation: Validation[Constraint.Primitive.Text, CIString]): Self[CIString] =
    operation.string(validation = validation.contramap(CIString.apply)).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring(validation = Validation.valid)

  def cistring(
      minimum: Undefined.Or[Int] = Undefined,
      maximum: Undefined.Or[Int] = Undefined,
      matches: Undefined.Or[Pattern] = Undefined
  ): Self[CIString] = self.cistring(validation = validation.cistring(minimum, maximum, matches))

  extension (x: cistring.type)
    def matches(reference: CIString): Self[CIString] = self.cistring(validation = validation.cistring.equals(reference))

    def required(
        maximum: Undefined.Or[Int] = Undefined,
        matches: Undefined.Or[Pattern] = Undefined
    ): Self[CIString] = self.cistring(minimum = 1, maximum, matches)

    def required: Self[CIString] = required()

    def nonEmpty: Self[Option[CIString]] = self.cistring.imap(_.some.filter(_.nonEmpty))(_.getOrElse(CIString.empty))
