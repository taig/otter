package io.taig.otter.openapi

import io.taig.otter.codecs.*
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.*

import scala.annotation.tailrec

val toSchema: Codec[?] => Schema =
  case codec: Collection[?]  => toSchema(codec)
  case codec: Coproduct[?]   => toSchema(codec)
  case codec: Dictionary[?]  => toSchema(codec)
  case codec: Dynamic[?]     => toSchema(codec)
  case codec: Enumeration[?] => toSchema(codec)
  case codec: Primitive[?]   => toSchema(codec)
  case codec: Product[?]     => ???
  case codec: Record[?]      => toSchema(codec)
  case codec: Union[?]       => toSchema(codec)

def toSchema(codec: Collection[?]): Schema = Schema.Array(items = toSchema(codec.codec))

def toSchema(codec: Coproduct[?]): Schema =
  val codecs = codec.toNonEmptyChain.toChain.map: branch =>
    codec.discriminator match
      case Discriminator.Nested(identifier, value) =>
        Schema.Object(properties = Chain(identifier -> toSchema(string), value -> toSchema(branch.codec)))
      case Discriminator.Merged(identifier)        =>
        val properties = toSchema(branch.codec) match
          case schema: Schema.Object => schema.properties
          case _ => Chain.empty
        Schema.Object(properties = Chain(identifier -> toSchema(string)) ++ properties)
      case Discriminator.Keyed => Schema.Object(properties = Chain(branch.name -> toSchema(branch.codec)))

  Schema.OneOf(codecs)

def toSchema(codec: Dictionary[?]): Schema = Schema.Value(
  tpe = "object",
  additionalProperties = Some(toSchema(codec.codec))
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

def toSchema(codec: Record[?]): Schema = Schema.Object(
  description = codec.description,
  properties = codec.toChain.map(field => field.name -> toSchema(field.codec)),
  required = codec.toChain.collect { case field if !field.isOptional => field.name }
)

def toSchema(codec: Union[?]): Schema = Schema.OneOf(
  codecs = codec.toNonEmptyChain.map(toSchema).toChain
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
