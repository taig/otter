package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Violation

object JsonCirceDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](schema: Json[A], json: CirceJson): Validated[Violations, A] = schema match
    case Json.Coerce(annotation)     => CoerceDecoder(decoder = this).decode(schema = annotation.self, json)
    case Json.Collection(annotation) =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(CollectionDecoder(decoder = this).decode(schema = annotation.self, _))
    case Json.Constant(annotation) =>
      ConstantDecoder(decoder = this, encoder = JsonCirceEncoder, render = _.toData)
        .decode(schema = annotation.self, json)
    case Json.Dictionary(annotation) =>
      json.asObject
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .map(_.toList)
        .andThen(DictionaryDecoder(decoder = this).decode(schema = annotation.self, _))
    case Json.Enumeration(annotation) =>
      EnumerationDecoder(decoder = this, encoder = JsonCirceEncoder, render = _.toData)
        .decode(schema = annotation.self, json)
    case Json.Nullable(annotation) =>
      NullableDecoder(decoder = this, empty = _.isNull).decode(schema = annotation.self, json)
    case schema: Json.Primitive[?] => JsonPrimitiveCirceDecoder.decode(schema, json)
    case Json.Record(annotation)   =>
      json.asObject
        .map(_.toList)
        .map(Chain.fromSeq)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(RecordDecoder(decoder = JsonFieldCirceDecoder).decode(annotation.self, _))
    case Json.Tuple(annotation) =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(TupleDecoder(decoder = this, empty = _.isNull).decode(schema = annotation.self, _))
    case Json.Union(annotation) =>
      UnionDecoder(decoder = this).decode(schema = annotation.self, json)
