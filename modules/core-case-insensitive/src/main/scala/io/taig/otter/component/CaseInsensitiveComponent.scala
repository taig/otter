package io.taig.otter.component

import io.taig.validation.Validation
import io.taig.validation
import io.taig.validation.Constraint
import org.typelevel.ci.CIString
import io.taig.otter.operation.StringSchemaInvariant
import io.taig.Undefined
import java.util.regex.Pattern
import cats.syntax.all.*

trait CaseInsensitiveComponent[+Self[_]](using schema: StringSchemaInvariant[Self, ?]):
  self =>

  def cistring(
      validation: Validation[Constraint.Primitive.Text, CIString]
  ): Self[CIString] = schema.string(validation = validation.contramap(CIString.apply)).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring(validation = Validation.valid)

  extension (x: cistring.type)
    def apply(
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined,
        matches: Undefined.Or[Pattern] = Undefined
    ): Self[CIString] = self.cistring(validation = validation.cistring(minimum, maximum, matches))

    def matches(reference: CIString): Self[CIString] = self.cistring(validation = validation.cistring.equals(reference))

    def required(
        maximum: Undefined.Or[Int] = Undefined,
        matches: Undefined.Or[Pattern] = Undefined
    ): Self[CIString] = apply(minimum = 1, maximum, matches)

    def required: Self[CIString] = required()

    def nonEmpty: Self[Option[CIString]] = self.cistring.imap(_.some.filter(_.nonEmpty))(_.getOrElse(CIString.empty))
