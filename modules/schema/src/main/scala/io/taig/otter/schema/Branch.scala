package io.taig.otter.schema

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violation

sealed abstract class Branch[A]:
  def key: String
  def schema: Schema[A]

  def decode(openapi: Option[OpenApi.Value], discriminator: Discriminator): Validated[Violations, Option[A]]
  def encode(a: A, discriminator: Discriminator): Option[OpenApi.Value]

  def :+[B](other: Branch[B]): Coproduct[A + B] = toCoproduct :+ other
  def +:[B](other: Branch[B]): Coproduct[B + A] = other +: toCoproduct

  def toCoproduct: Coproduct[A] = Coproduct(this)
  def to[B](using Evidence.Coproduct.Aux[B, A]): Coproduct[B] = toCoproduct.to[B]

object Branch:
  extension [A <: Matchable](self: Branch[A])
    inline def |[B <: Matchable](branch: Branch[B]): Coproduct[A | B] = (self :+ branch).imap[A | B] {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

  def apply[A, B](name: A, a: => Schema.Value[A], b: => Schema[B]): Branch[B] = new Branch[B]:
    override def key: String = a.print(name).orEmpty
    override def schema: Schema[B] = b

    override def decode(
        openapi: Option[OpenApi.Value],
        discriminator: Discriminator
    ): Validated[Violations, Option[B]] = discriminator match
      case Discriminator.Nested(identifier, value) =>
        openapi match
          case Some(OpenApi.Object(values)) =>
            values
              .get(identifier)
              .toValid(Violations.rootNec(Violation.required))
              .andThen(a.decode)
              .leftMap(_.modifyHistory(identifier /: _))
              .map(a.print(_).orEmpty)
              .andThen: a =>
                if a == key
                then b.decode(values.getOrElse(value, OpenApi.Null)).leftMap(_.modifyHistory(value /: _)).map(_.some)
                else none.valid
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).invalid
          case None          => Violations.rootNec(Violation.required).invalid
      case Discriminator.Merged(identifier) =>
        openapi match
          case Some(openapi @ OpenApi.Object(values)) =>
            values
              .get(identifier)
              .toValid(Violations.rootNec(Violation.required))
              .andThen(a.decode)
              .leftMap(_.modifyHistory(identifier /: _))
              .map(a.print(_).orEmpty)
              .andThen(a => if a == key then b.decode(openapi).map(_.some) else none.valid)
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).invalid
          case None          => Violations.rootNec(Violation.required).invalid
      case Discriminator.Keyed =>
        openapi match
          case Some(OpenApi.Object(values)) =>
            b.decode(values.getOrElse(key, OpenApi.Null)).leftMap(_.modifyHistory(key /: _)).map(_.some)
          case Some(openapi) => Violations.rootNec(Violation.tpe("object", openapi.tpe)).invalid
          case None          => Violations.rootNec(Violation.required).invalid
      case Discriminator.None =>
        schema.decode(openapi) match
          case Validated.Valid(b)            => b.some.valid
          case Validated.Invalid(violations) => violations.modifyHistory(key /: _).invalid

    override def encode(b: B, discriminator: Discriminator): Option[OpenApi.Value] = discriminator match
      case Discriminator.Nested(identifier, value) => OpenApi.obj(identifier := key, value := schema.encode(b)).some
      case Discriminator.Merged(identifier) =>
        val payload = schema.encode(b).flatMap(_.asObject).getOrElse(OpenApi.Object.Empty)
        (payload ++ OpenApi.obj(identifier := key)).some
      case Discriminator.Keyed => OpenApi.obj(key := schema.encode(b)).some
      case Discriminator.None  => schema.encode(b)
