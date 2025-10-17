package io.taig.otter.component

import io.taig.otter.operation.StringOperation
import io.taig.validation
import io.github.iltotore.iron.{Constraint as _, *}
import io.github.iltotore.iron.constraint.all.*
import cats.syntax.all.*
import cats.Invariant
import io.taig.validation.Validation
import scala.compiletime.*
import java.util.regex.Pattern

trait IronStringComponent[+Self[_]: Invariant](using operation: StringOperation[Self]):
  val string: Self[String] = operation.string(Validation.valid)

  def apply(minimum: Int, maximum: Int): Self[String :| (MinLength[minimum.type] & MaxLength[maximum.type])] =
    operation.string(validation.std.string(minimum = minimum , maximum = maximum))
      .imap[String :| (MinLength[minimum.type] & MaxLength[maximum.type])](
        _.assume[MinLength[minimum.type] & MaxLength[maximum.type]]
      )(_.toString)

  extension (self: string.type)
    def minimum(reference: Int): Self[String :| MinLength[reference.type]] =
      operation.string(validation.std.string.minimum(reference))
        .imap[String :| MinLength[reference.type]](_.assume[MinLength[reference.type]])(_.toString)

    def maximum(reference: Int): Self[String :| MaxLength[reference.type]] =
      operation.string(validation.std.string.maximum(reference))
        .imap[String :| MaxLength[reference.type]](_.assume[MaxLength[reference.type]])(_.toString)

    def length(reference: Int): Self[String :| FixedLength[reference.type]] =
      operation.string(validation.std.string(minimum = reference , maximum = reference))
        .imap[String :| FixedLength[reference.type]](_.assume[FixedLength[reference.type]])(_.toString)

    def matches(pattern: String): Self[String :| Match[pattern.type]] =
      operation.string(validation.std.string.matches(Pattern.compile(pattern)))
        .imap[String :| Match[pattern.type]](_.assume[Match[pattern.type]])(_.toString)