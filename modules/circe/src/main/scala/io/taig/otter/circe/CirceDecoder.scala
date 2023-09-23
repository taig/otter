package io.taig.otter.circe

import cats.syntax.all.*
import cats.data.{Ior, Validated}
import io.circe.{Json, JsonObject}
import io.taig.otter.Schema.{Coproduct, Record}
import io.taig.otter.{+, Null, Branch, Decoder, Discriminator, Schema}
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
      case schema: Schema.Record[?]        => record.decode(schema, json)

  val value: Decoder[Schema.Value, Json] = new Decoder:
    override def decode[B](schema: Schema.Value[B], json: Json): Validated[Violations, B] = ???

  val coproduct: Decoder[Schema.Coproduct, Json] = new Decoder[Schema.Coproduct, Json]:
    override def decode[B](schema: Schema.Coproduct[B], json: Json): Validated[Violations, B] =
      decode(schema, json, schema.discriminator) match
        case Ior.Left(violations)       => violations.invalid
        case Ior.Right(Some(b))         => b.valid
        case Ior.Right(None)            => ???
        case Ior.Both(_, Some(b))       => b.valid
        case Ior.Both(violations, None) => violations.invalid

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
        case Discriminator.None                      =>
          CirceDecoder.schema.decode(branch.value, json).toIor.map(_.some)

  val record: Decoder[Schema.Record, Json] = new Decoder:
    override def decode[A](schema: Schema.Record[A], json: Json): Validated[Violations, A] =
      decode(schema, json, schema.nulls)

    def decode[A](schema: Schema.Record[A], json: Json, nulls: Null): Validated[Violations, A] = schema match
      case Record.Empty(description, example, nulls) => ???
      case Record.Root(field, description, example, nulls) => ???
      case Record.Zip(left, right, _, _, _) => ???
      case Record.Optional(self) =>
        if json.isNull then none.valid[Violations] else decode(self, json).map(_.some)
      case Record.Validate(self, validation, _) => decode(self, json).andThen(validation(_).leftMap(Violations.root))

