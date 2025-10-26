package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.operation.StringOperation

import java.util.UUID
import io.taig.validation.Validation
import io.taig.otter.Constraint

trait StringComponent[+Self[_]: Invariant](using operation: StringOperation[Self]):
  self =>

  final def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String] =
    operation.string(validation)

  final val string: Self[String] = operation.string(Validation.valid)

  // final def string(
  //     minimum: Undefined.Or[Int] = Undefined,
  //     maximum: Undefined.Or[Int] = Undefined,
  //     matches: Undefined.Or[Pattern] = Undefined
  // ): Self[String] = self.string(validation = validation.std.string(minimum, maximum, matches))

  // extension (x: string.type)
  //   def matches(reference: String): Self[String] = self.string(validation = validation.std.string.equals(reference))

  //   def required(
  //       maximum: Undefined.Or[Int] = Undefined,
  //       matches: Undefined.Or[Pattern] = Undefined
  //   ): Self[String] = self.string(minimum = 1, maximum, matches)

  //   def required: Self[String] = required()

  //   def nonEmpty: Self[Option[String]] = self.string.imap(_.some.filter(_.nonEmpty))(_.getOrElse(""))

  final def parser[A](name: String)(f: String => Either[String, A])(g: A => String): Self[A] =
    operation.parser(name, decode = f, encode = g)

  final val uuid: Self[UUID] = parser(name = "uuid") { value =>
    Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
  }(_.show)

  // final val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
