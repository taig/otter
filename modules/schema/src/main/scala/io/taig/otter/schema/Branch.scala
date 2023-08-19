package io.taig.otter.schema

import cats.data.{Ior, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violation

final case class Branch[A, B](name: A, key: Schema.Value[A], schema: Schema[B]):
  def :+[C, D](branch: Branch[C, D]): Coproduct[B + D] = toCoproduct :+ branch
  def +:[C, D](branch: Branch[C, D]): Coproduct[D + B] = branch +: toCoproduct

  def printName: String = key.print(name).orEmpty

  def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Ior[Violations, Option[B]] =
    discriminator match
      case Discriminator.Nested(identifier, value) =>
        openapi match
          case Some(OpenApi.Object(values)) =>
            values
              .get(identifier)
              .toValid(Violations.rootNec(Violation.required))
              .andThen(key.decode)
              .leftMap(_.modifyHistory(identifier /: _))
              .map(key.print(_).orEmpty)
              .andThen: a =>
                if a == printName
                then
                  schema.decode(values.getOrElse(value, OpenApi.Null)).leftMap(_.modifyHistory(value /: _)).map(_.some)
                else none.valid
              .toIor
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).leftIor
          case None          => Violations.rootNec(Violation.required).leftIor
      case Discriminator.Merged(identifier) =>
        openapi match
          case Some(openapi @ OpenApi.Object(values)) =>
            values
              .get(identifier)
              .toValid(Violations.rootNec(Violation.required))
              .andThen(key.decode)
              .leftMap(_.modifyHistory(identifier /: _))
              .map(key.print(_).orEmpty)
              .andThen(a => if a == printName then schema.decode(openapi).map(_.some) else none.valid)
              .toIor
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).leftIor
          case None          => Violations.rootNec(Violation.required).leftIor
      case Discriminator.Keyed =>
        openapi match
          case Some(OpenApi.Object(values)) =>
            values
              .get(printName)
              .match
                case Some(openapi) => schema.decode(openapi).toIor.map(_.some)
                case None          => Violations.rootNec(Violation.required).leftIor.putRight(none)
              .leftMap(_.modifyHistory(printName /: _))
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).leftIor
          case None          => Violations.rootNec(Violation.required).leftIor
      case Discriminator.None =>
        schema.decode(openapi) match
          case Validated.Valid(b) => b.some.rightIor
          case Validated.Invalid(violations) =>
            violations.modifyHistory(printName /: _).leftIor.putRight(none)

  def encode(b: B, discriminator: Discriminator): Option[OpenApi.Value] = discriminator match
    case Discriminator.Nested(identifier, value) =>
      OpenApi.obj(identifier := key.encode(name), value := schema.encode(b)).some
    case Discriminator.Merged(identifier) =>
      val payload = schema.encode(b).flatMap(_.asObject).getOrElse(OpenApi.Object.Empty)
      (payload ++ OpenApi.obj(identifier := key.encode(name))).some
    case Discriminator.Keyed => OpenApi.obj(printName := schema.encode(b)).some
    case Discriminator.None  => schema.encode(b)

  def toCoproduct: Coproduct[B] = Coproduct(this)
  def to[C](using Evidence.Coproduct.Aux[C, B]): Coproduct[C] = toCoproduct.to[C]

object Branch:
  extension [A, B <: Matchable](self: Branch[A, B])
    inline def |[C, D <: Matchable](branch: Branch[C, D]): Coproduct[B | D] = (self :+ branch).imap[B | D] {
      case Left(b)  => b
      case Right(d) => d
    } {
      case b: B => Left(b)
      case d: D => Right(d)
    }
