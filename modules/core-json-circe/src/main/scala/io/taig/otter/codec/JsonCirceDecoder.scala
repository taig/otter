package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toData
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Violation

object JsonCirceDecoder extends Decoder[Json.Node, CirceJson]:
  override def decode[R](schema: Json.Node[Nothing, R], json: CirceJson): Validated[Violations, R] =
    schema match
      case Json.Coerce.Schema(node)     => JsonCoerceCirceDecoder.decode(node.self, json)
      case Json.Collection.Schema(node) =>
        array(json).andThen(CollectionDecoder(this).decode(node.self, _))
      case Json.Constant.Schema(node) =>
        ConstantDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData).decode(node.self, json)
      case Json.Dictionary.Schema(node) =>
        obj(json).map(_.toList).andThen(DictionaryDecoder(this).decode(node.self, _))
      case Json.Enumeration.Schema(node) =>
        EnumerationDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData).decode(node.self, json)
      case Json.Optional.Schema(node) =>
        OptionalDecoder(this, empty = _.isNull).decode(node.self, json)
      case schema @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveCirceDecoder.decode(schema, json)
      case schema @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveCirceDecoder.decode(schema, json)
      case schema @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveCirceDecoder.decode(schema, json)
      case Json.Record.Schema(node)                  =>
        obj(json)
          .map(values => Chain.fromSeq(values.toList))
          .andThen(RecordDecoder(JsonFieldCirceDecoder).decode(node.self, _))
      case Json.Tuple.Schema(node) =>
        array(json).andThen(values => TupleDecoder(this, empty = _.isNull).decode(node.self, values.toVector))
      case Json.Union.Schema(node) => UnionDecoder(JsonBranchCirceDecoder).decode(node.self, json)

  private def mismatch(name: String, json: CirceJson): Violations =
    Violations(Violation(constraint = Constraint.Generic.Type(name), actual = typeOf(json).asData, hint = none))

  private def array(json: CirceJson): Validated[Violations, Vector[CirceJson]] =
    json.asArray.toValid(mismatch("array", json))

  private def obj(json: CirceJson): Validated[Violations, io.circe.JsonObject] =
    json.asObject.toValid(mismatch("object", json))
