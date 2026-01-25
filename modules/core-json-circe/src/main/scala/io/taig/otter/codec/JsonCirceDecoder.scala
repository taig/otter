package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toData
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Violation

object JsonCirceDecoder extends Decoder[Json.Read, CirceJson]:
  override def decode[A](schema: Json.Read[A], json: CirceJson): Validated[Violations, A] = schema match
    case self: Json.Coerce.Read[A]     => JsonCoerceCirceDecoder.decode(schema = self, json)
    case self: Json.Collection.Read[A] =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(CollectionDecoder(decoder = this).decode(schema = self.self.self, _))
    case self: Json.Constant.Read[A] =>
      ConstantDecoder(decoder = this, encoder = JsonCirceEncoder, render = _.toData)
        .decode(schema = self.self.self, json)
    case self: Json.Dictionary.Read[A] =>
      json.asObject
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .map(_.toList)
        .andThen(DictionaryDecoder(decoder = this).decode(schema = self.self.self, _))
    case self: Json.Enumeration.Read[A] =>
      EnumerationDecoder(decoder = this, encoder = JsonCirceEncoder, render = _.toData)
        .decode(schema = self.self.self, json)
    case self: Json.Optional.Read[A] =>
      OptionalDecoder(decoder = this, empty = _.isNull).decode(schema = self.self.self, json)
    case schema: Json.Primitive.Read[A] => JsonPrimitiveCirceDecoder.decode(schema, json)
    case self: Json.Record.Read[A]      =>
      json.asObject
        .map(_.toList)
        .map(Chain.fromSeq)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "object"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(RecordDecoder(decoder = JsonFieldCirceDecoder).decode(self.self.self, _))
    case self: Json.Tuple.Read[A] =>
      json.asArray
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "array"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(TupleDecoder(decoder = this, empty = _.isNull).decode(schema = self.self.self, _))
    case self: Json.Union.Read[A] =>
      UnionDecoder(decoder = JsonBranchCirceDecoder).decode(schema = self.self.self, json)
