package io.taig.otter.openapi

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

def toSchema(codec: Coproduct[?]): Schema = Schema.OneOf(
  codec.toNonEmptyChain.map(branch => toSchema(branch.codec)).toChain
)

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
