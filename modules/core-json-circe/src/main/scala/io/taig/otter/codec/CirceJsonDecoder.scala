package io.taig.otter.codec

import io.circe.Json as CirceJson
import cats.syntax.all.*
import cats.data.Validated
import io.taig.data.circe.*
import io.taig.otter.typeOf
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.Constraint
import cats.data.Chain
import io.taig.validation.Violation

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](schema: Json[A], json: CirceJson): Validated[Violations, A] = schema match
    case Json.Coerce(schema)     => CoerceDecoder(decoder = this).decode(schema = schema.self, json)
    case Json.Collection(schema) =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(CollectionDecoder(decoder = this).decode(schema = schema.self, _))
    case Json.Constant(schema) =>
      ConstantDecoder(decoder = this, encoder = CirceJsonEncoder, render = _.toData).decode(schema = schema.self, json)
    case Json.Dictionary(schema) =>
      json.asObject
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .map(_.toList)
        .andThen(DictionaryDecoder(decoder = this).decode(schema = schema.self, _))
    case Json.Enumeration(schema) =>
      EnumerationDecoder(decoder = this, encoder = CirceJsonEncoder, render = _.toData)
        .decode(schema = schema.self, json)
    case Json.Nullable(schema)     => ???
    case schema: Json.Primitive[?] => CirceJsonPrimitiveDecoder.decode(schema, json)
    case Json.Record(schema)       =>
      json.asObject
        .map(_.toList)
        .map(Chain.fromSeq)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(RecordDecoder(decoder = CirceJsonFieldDecoder).decode(schema.self, _))
    case Json.Tuple(schema) =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(TupleDecoder(decoder = this, empty = _.isNull).decode(schema = schema.self, _))
    case Json.Union(schema) =>
      UnionDecoder(decoder = this).decode(schema = schema.self, json)
