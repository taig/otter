package io.taig.otter.openapi

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.{Discriminator as OtterDiscriminator, *}

import scala.annotation.tailrec

def toSchemaOr(codec: Codec[?], to: Codec[?] => Schema | Reference): Schema = codec match
  case codec: Collection[?]  => toSchema(codec, to)
  case codec: Coproduct[?]   => toSchema(codec, to)
  case codec: Dictionary[?]  => toSchema(codec, to)
  case codec: Dynamic[?]     => toSchema(codec)
  case codec: Enumeration[?] => toSchema(codec)
  case codec: Primitive[?]   => toSchema(codec)
  case codec: Product[?]     => ???
  case codec: Record[?]      => toSchema(codec, to)
  case codec: Union[?]       => toSchema(codec, to)

def toSchema(codec: Codec[?]): Schema = toSchemaOr(codec, toSchema)

def toSchemaOrReference(codec: Codec[?]): Schema | Reference = codec.name match
  case Some(name) => Reference(ref = s"components/schemas/$name")
  case None       => toSchemaOr(codec, toSchemaOrReference)

def toSchema(codec: Collection[?], to: Codec[?] => Schema | Reference): Schema =
  Schema.Array(items = to(codec.codec))

def toSchema(codec: Coproduct[?], to: Codec[?] => Schema | Reference): Schema =
  val codecs = codec.toNonEmptyChain.toChain.map: branch =>
    codec.discriminator match
      case OtterDiscriminator.Nested(identifier, value) =>
        Schema.Object(
          description = branch.codec.description,
          properties = Chain(identifier -> to(branch.key), value -> to(branch.codec.description(none))),
          required = Chain.one(identifier) ++ (if codec.isOptional then Chain.empty else Chain.one(value))
        )
      case OtterDiscriminator.Merged(identifier) =>
        to(branch.codec.description(none)) match
          case schema: Schema.Object =>
            Schema.Object(
              description = branch.codec.description,
              properties = Chain(identifier -> to(branch.key)) ++ schema.properties,
              required = Chain(identifier) ++ schema.required
            )
          case _ =>
            Schema.Object(
              description = branch.codec.description,
              properties = Chain(identifier -> to(branch.key)),
              required = Chain(identifier)
            )
      case OtterDiscriminator.Keyed =>
        Schema.Object(
          description = branch.codec.description,
          properties = Chain(branch.name -> to(branch.codec.description(none))),
          required = Chain(branch.name)
        )

  val discriminator = codec.discriminator match
    case OtterDiscriminator.Nested(identifier, _) =>
      Some(Discriminator(propertyName = identifier))
    case OtterDiscriminator.Merged(identifier) =>
      Some(Discriminator(propertyName = identifier))
    case OtterDiscriminator.Keyed => None

  Schema.OneOf(codecs, discriminator = discriminator)

def toSchema(codec: Dictionary[?], to: Codec[?] => Schema | Reference): Schema = Schema.Value(
  tpe = "object",
  additionalProperties = Some(to(codec.codec))
)

def toSchema(codec: Dynamic[?]): Schema = Schema.Value(tpe = "object", description = codec.description)

def toSchema(codec: Enumeration[?]): Schema = Schema.Enumeration(
  tpe = typeOf(codec.codec),
  enums = codec.values
)

def toSchema(codec: Primitive[?]): Schema = Schema.Value(
  tpe = typeOf(codec.tpe),
  format = codec.format,
  description = codec.description
)

def toSchema(codec: Record[?], to: Codec[?] => Schema | Reference): Schema = Schema.Object(
  description = codec.description,
  properties = codec.toChain.map(field => field.name -> to(field.codec)),
  required = codec.toChain.collect { case field if !field.isOptional => field.name }
)

def toSchema(codec: Union[?], to: Codec[?] => Schema | Reference): Schema = Schema.OneOf(
  codecs = codec.toNonEmptyChain.map(to).toChain
)

@tailrec
def typeOf(codec: Value[?]): String = codec match
  case codec: Enumeration[?] => typeOf(codec.codec)
  case codec: Primitive[?]   => typeOf(codec.tpe)

val typeOf: Type[?] => String =
  case Type.Double | Type.Float | Type.BigDecimal => "number"
  case Type.Int | Type.Long | Type.BigInt         => "integer"
  case Type.Boolean                               => "boolean"
  case Type.String                                => "string"
