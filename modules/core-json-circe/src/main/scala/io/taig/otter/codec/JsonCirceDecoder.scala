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

object JsonCirceDecoder extends Decoder[Json.Of, CirceJson]:
  override def decode[R](schema: Json.Of[Nothing, R], json: CirceJson): Validated[Violations, R] =
    (schema: @unchecked) match
      case schema: Json.Coerce.Of[?, R]     => JsonCoerceCirceDecoder.decode(schema.self.self, json)
      case schema: Json.Collection.Of[?, R] =>
        array(json).andThen(CollectionDecoder(this).decode(schema.self.self, _))
      case schema: Json.Constant.Of[?, R] =>
        ConstantDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData)
          .decode(schema.self.self, json)
      case schema: Json.Dictionary.Of[?, R] =>
        obj(json).map(_.toList).andThen(DictionaryDecoder(this).decode(schema.self.self, _))
      case schema: Json.Enumeration.Of[?, R] =>
        EnumerationDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData)
          .decode(schema.self.self, json)
      case schema: Json.Optional.Of[?, R] =>
        OptionalDecoder(this, empty = _.isNull).decode(schema.self.self, json)
      case schema: Json.Primitive.Of[?, R] => JsonPrimitiveCirceDecoder.decode(schema, json)
      case schema: Json.Record.Of[?, R]    =>
        obj(json)
          .map(values => Chain.fromSeq(values.toList))
          .andThen(RecordDecoder(JsonFieldCirceDecoder).decode(schema.self.self, _))
      case schema: Json.Tuple.Of[?, R] =>
        array(json).andThen(values => TupleDecoder(this, empty = _.isNull).decode(schema.self.self, values.toVector))
      case schema: Json.Union.Of[?, R] => UnionDecoder(JsonBranchCirceDecoder).decode(schema.self.self, json)

  private def mismatch(name: String, json: CirceJson): Violations =
    Violations(Violation(constraint = Constraint.Generic.Type(name), actual = typeOf(json).asData, hint = none))

  private def array(json: CirceJson): Validated[Violations, Vector[CirceJson]] =
    json.asArray.toValid(mismatch("array", json))

  private def obj(json: CirceJson): Validated[Violations, io.circe.JsonObject] =
    json.asObject.toValid(mismatch("object", json))
