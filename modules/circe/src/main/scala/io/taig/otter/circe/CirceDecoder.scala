package io.taig.otter.circe

import cats.syntax.all.*
import cats.data.{Chain, Ior, Validated}
import io.circe.{Json, JsonObject}
import io.taig.otter.Schema.{Coproduct, Record}
import io.taig.otter.{+, Branch, Decoder, Discriminator, Field, Null, Schema}
import io.taig.otter.validation.{Violation, Violations}

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder:
    override def decode[B](schema: Schema[B], json: Json): Validated[Violations, B] = schema match
      case schema: Schema.Value[?]         => value.decode(schema, json)
      case schema: Schema.Collection[?, ?] => ???
      case schema: Schema.Coproduct[?]     => coproduct.decode(schema, json)
      case schema: Schema.Dictionary[?]    => ???
      case schema: Schema.Dynamic[?]       => ???
      case schema: Schema.Product[?]       => ???
      case schema: Schema.Record[?] =>
        if json.isNull
        then record.decode(schema, none)
        else
          json.asObject
            .toValid(Violations.rootNec(Violation.tpe("object", actual = name(json))))
            .andThen(obj => record.decode(schema, Chain.fromIterableOnce(obj.toIterable).some))

  val value: Decoder[Schema.Value, Json] = new Decoder:
    override def decode[B](schema: Schema.Value[B], json: Json): Validated[Violations, B] = ???

  val coproduct: Decoder[Schema.Coproduct, Json] = new Decoder[Schema.Coproduct, Json]:
    override def decode[B](schema: Schema.Coproduct[B], json: Json): Validated[Violations, B] =
      decode(schema, json, schema.discriminator) match
        case Ior.Left(violations) => violations.invalid
        case Ior.Right(Some(b))   => b.valid
        case Ior.Right(None)      =>
          // validation error suckers
          ???
        case Ior.Both(violations, b) => b.toValid(violations)

    def decode[B](schema: Schema.Coproduct[B], json: Json, discriminator: Discriminator): Ior[Violations, Option[B]] =
      schema match
        case Coproduct.Root(branch, _, _, _) => decode(branch, json, discriminator)
        case Coproduct.Optional(self)        => decode(self, json, discriminator).map(_.some)
        case Coproduct.Validate(self, validation, _) =>
          decode(self, json, discriminator).flatMap(_.traverse(validation(_).leftMap(Violations.root).toIor))
        case schema: Coproduct.OrElse[a, b] => decode[a, b](schema, json, discriminator)

    def decode[A, B](
        schema: Schema.Coproduct.OrElse[A, B],
        json: Json,
        discriminator: Discriminator
    ): Ior[Violations, Option[A + B]] = decode(schema.left, json, discriminator) match
      case Ior.Left(violations) =>
        decode(schema.right, json, discriminator).map(_.map(_.asRight)).leftMap(violations |+| _)
      case Ior.Right(Some(a))   => a.asLeft.some.rightIor
      case Ior.Both(_, Some(b)) => b.asLeft.some.rightIor

    def decode[A, B](branch: Branch[A, B], json: Json, discriminator: Discriminator): Ior[Violations, Option[B]] =
      discriminator match
        case Discriminator.Nested(identifier, value) => ???
        case Discriminator.Merged(identifier)        => ???
        case Discriminator.Keyed                     => ???
        case Discriminator.None =>
          CirceDecoder.schema.decode(branch.value, json).toIor.map(_.some)

  val record: Decoder[Schema.Record, Option[Chain[(String, Json)]]] = new Decoder.WithRemainders:
    override def decode[A](schema: Schema.Record[A], json: Option[Chain[(String, Json)]]): Validated[Violations, A] =
      // TODO allow to fail on unknown object properties
      decodeWithRemainders(schema, json).map(_._2)

    override def decodeWithRemainders[B](
        schema: Record[B],
        json: Option[Chain[(String, Json)]]
    ): Validated[Violations, (Option[Chain[(String, Json)]], B)] =
      schema match
        case Record.Empty(_, _, _)       => (json, ()).valid
        case Record.Root(field, _, _, _) => decodeWithRemainders(field, json)
        case Record.Zip(left, right, _, _, _) =>
          decodeWithRemainders(left, json) match
            case Validated.Valid((json, a)) =>
              decodeWithRemainders(right, json) match
                case Validated.Valid((json, b)) => (json, (a, b)).valid
        case Record.Optional(self) =>
          decodeWithRemainders(self, json).map(_.map(_.some))
        case Record.Validate(self, validation, _) =>
          decodeWithRemainders(self, json).andThen(_.traverse(validation(_).leftMap(Violations.root)))

    // is an object optional when all of its fields are optional????
    def decodeWithRemainders[A, B](
        field: Field[A, B],
        json: Option[Chain[(String, Json)]]
    ): Validated[Violations, (Option[Chain[(String, Json)]], B)] = ???
