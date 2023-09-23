package io.taig.otter.circe

import cats.syntax.all.*
import cats.data.{Ior, Validated}
import io.circe.Json
import io.taig.otter.Schema.Coproduct
import io.taig.otter.{+, Branch, Decoder, Discriminator, Schema}
import io.taig.otter.validation.Violations

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder:
    override def decode[B](schema: Schema[B], json: Json): Validated[Violations, B] = schema match
      case schema: Schema.Value[?]         => value.decode(schema, json)
      case schema: Schema.Collection[?, ?] => ???
      case schema: Schema.Coproduct[?]     => coproduct.decode(schema, json)
      case schema: Schema.Dictionary[?]    => ???
      case schema: Schema.Dynamic[?]       => ???
      case schema: Schema.Product[?]       => ???
      case schema: Schema.Record[?]        => ???

  val value: Decoder[Schema.Value, Json] = new Decoder:
    override def decode[B](schema: Schema.Value[B], json: Json): Validated[Violations, B] = ???

  val coproduct: Decoder[Schema.Coproduct, Json] = new Decoder[Schema.Coproduct, Json]:
    override def decode[B](schema: Schema.Coproduct[B], json: Json): Validated[Violations, B] =
      decode(schema, json, schema.discriminator)

    def decode[B](schema: Schema.Coproduct[B], json: Json, discriminator: Discriminator): Validated[Violations, B] =
      schema match
        case Coproduct.Root(branch, _, _, _) => ???
        case Coproduct.Optional(self)        => decode(self, json, discriminator).map(_.some)
        case Coproduct.Validate(self, validation, _) =>
          decode(self, json, discriminator).andThen(validation(_).leftMap(Violations.root))
        case schema: Coproduct.OrElse[a, b] => decode[a, b](schema, json, discriminator)

    def decode[A, B](
        schema: Schema.Coproduct.OrElse[A, B],
        json: Json,
        discriminator: Discriminator
    ): Validated[Violations, A + B] = decode(schema.left, json, discriminator)
      .map(_.asLeft)
      .orElse(decode(schema.right, json, discriminator).map(_.asRight))

    def decode[A, B](branch: Branch[A, B], json: Json, discriminator: Discriminator): Ior[Violations, Option[B]] =
      discriminator match
        case Discriminator.Nested(identifier, value) => ???
        case Discriminator.Merged(identifier) => ???
        case Discriminator.Keyed => ???
        case Discriminator.None => ???
