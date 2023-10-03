package io.taig.otter

import cats.Eq
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.{History, Violation, Violations}

final case class Branch[A: Eq, B](name: A, key: Value.Required[A], codec: Codec[B]):
  def :+[C](branch: Branch[?, C]): Coproduct[Either[B, C]] = toCoproduct :+ branch
  def +:[C](branch: Branch[?, C]): Coproduct[Either[C, B]] = branch +: toCoproduct

  def toCoproduct: Coproduct[B] = Coproduct(this)

  def print: String = key.print(name)

  def decode(data: Chain[(String, Data)], discriminator: Discriminator): Validated[Violations, Option[B]] =
    discriminator match
      case Discriminator.Nested(identifier, value) =>
        data.firstWithRemainders(identifier) match
          case Some((identifier, data)) =>
            key.decode(identifier) match
              case Validated.Valid(identifier) if identifier === name =>
                data.first(value) match
                  case Some(data) => codec.decode(data).map(_.some)
                  case None       => ???
              case Validated.Valid(_)            => none.valid
              case Validated.Invalid(violations) => ???
          case None => Violations.oneNec(History.Root / identifier, Violation.required).invalid
      case Discriminator.Merged(identifier) => ???
      case Discriminator.Keyed              => ???

  def encode(b: B, discriminator: Discriminator): Chain[(String, Data)] = discriminator match
    case Discriminator.Nested(identifier, value) =>
      Chain(identifier -> key.encode(name), value -> codec.encode(b))
    case Discriminator.Merged(identifier) =>
      Chain.one(identifier -> key.encode(name)) ++ codec.encode(b).asObject.map(_.values).orEmpty
    case Discriminator.Keyed => Chain.one(print -> codec.encode(b))
