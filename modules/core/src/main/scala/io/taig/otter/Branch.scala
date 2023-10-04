package io.taig.otter

import cats.Eq
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.{History, Violation, Violations}

sealed abstract class Branch[A]:
  def key: Value.Required[?]
  def codec: Codec[?]
  def name: String

  def :+[B](branch: Branch[B]): Coproduct[Either[A, B]] = toCoproduct :+ branch
  def +:[B](branch: Branch[B]): Coproduct[Either[B, A]] = branch +: toCoproduct

  def toCoproduct: Coproduct[A] = Coproduct(this)

  def decode(data: Chain[(String, Data)], discriminator: Discriminator): Validated[Violations, Option[A]]
  def encode(a: A, discriminator: Discriminator): Chain[(String, Data)]

object Branch:
  def apply[A: Eq, B](a: A, ofKey: Value.Required[A], ofCodec: Codec[B]): Branch[B] = new Branch[B]:
    override def key: Value.Required[?] = ofKey
    override def codec: Codec[?] = ofCodec
    override def name: String = ofKey.print(a)

    override def decode(data: Chain[(String, Data)], discriminator: Discriminator): Validated[Violations, Option[B]] =
      discriminator match
        case Discriminator.Nested(identifier, value) =>
          data.firstWithRemainders(identifier) match
            case Some((identifier, data)) =>
              ofKey.decode(identifier) match
                case Validated.Valid(identifier) if identifier === a =>
                  data.first(value) match
                    case Some(data) => ofCodec.decode(data).map(_.some)
                    case None       => ???
                case Validated.Valid(_)            => none.valid
                case Validated.Invalid(violations) => ???
            case None => Violations.oneNec(History.Root / identifier, Violation.required).invalid
        case Discriminator.Merged(identifier) => ???
        case Discriminator.Keyed              => ???

    override def encode(b: B, discriminator: Discriminator): Chain[(String, Data)] = discriminator match
      case Discriminator.Nested(identifier, value) =>
        Chain(identifier -> ofKey.encode(a), value -> ofCodec.encode(b))
      case Discriminator.Merged(identifier) =>
        Chain.one(identifier -> ofKey.encode(a)) ++ ofCodec.encode(b).asObject.map(_.values).orEmpty
      case Discriminator.Keyed => Chain.one(name -> ofCodec.encode(b))
