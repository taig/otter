package io.taig.otter.codec

import io.circe.Json as CirceJson
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violation
import io.taig.data.circe.*
import io.taig.otter.typeOf
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.Constraint

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](schema: Json[A], json: CirceJson): Validated[Violations, A] = schema match
    case Json.Coerce(schema)     => CoerceDecoder(decoder = this).decode(schema = schema.self, json)
    case Json.Collection(schema) => ???
    case Json.Constant(schema)   =>
      ConstantDecoder(decoder = this, encoder = CirceJsonEncoder, render = _.toData).decode(schema = schema.self, json)
    case Json.Dictionary(schema)  => ???
    case Json.Enumeration(schema) =>
      EnumerationDecoder(decoder = this, encoder = CirceJsonEncoder, render = _.toData)
        .decode(schema = schema.self, json)
    case Json.Nullable(schema)     => ???
    case schema: Json.Primitive[?] => CirceJsonPrimitiveDecoder.decode(schema, json)
    case Json.Record(schema)       => ???
    case Json.Tuple(schema)        =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen(TupleDecoder(decoder = this, empty = _.isNull).decode(schema = schema.self, _))
    case Json.Union(schema) =>
      UnionDecoder(decoder = this).decode(schema = schema.self, json)
