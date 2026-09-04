package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toData
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonCirce
import io.taig.otter.Violations
import io.taig.validation.Violation

object JsonCirceDecoder extends Decoder[Json.Node, CirceJson]:
  private val collection: CollectionDecoder[Json.Node, CirceJson] = CollectionDecoder(this)

  private val constant: ConstantDecoder[Json.Primitive.Node, CirceJson] =
    ConstantDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData)

  private val dictionary: DictionaryDecoder[Json.Primitive.Text.Node, Json.Node, CirceJson] =
    DictionaryDecoder(JsonTextDecoder, this)

  private val enumeration: EnumerationDecoder[Json.Primitive.Node, CirceJson] =
    EnumerationDecoder(JsonPrimitiveCirceDecoder, JsonPrimitiveCirceEncoder, _.toData)

  private val optional: OptionalDecoder[Json.Node, CirceJson] = OptionalDecoder(this, empty = _.isNull)

  private val tuple: TupleDecoder[Json.Node, CirceJson] = TupleDecoder(this, empty = _.isNull)

  /** Lazy, unlike its neighbours, because [[JsonFieldCirceDecoder]] holds a decoder of this object: a `val` here would
    * have the two initialising each other. The same goes for [[JsonBranchCirceDecoder]] below.
    */
  private lazy val record: RecordDecoder[Json.Field.Node, CirceJson] = RecordDecoder(JsonFieldCirceDecoder)

  private lazy val union: UnionDecoder[Json.Branch.Node, CirceJson] = UnionDecoder(JsonBranchCirceDecoder)

  override def decode[R](schema: Json.Node[Nothing, R], json: CirceJson): Validated[Violations, R] =
    schema match
      case Json.Coerce.Schema(node)                  => JsonCoerceCirceDecoder.decode(node.self, json)
      case Json.Collection.Schema(node)              => array(json).andThen(collection.decode(node.self, _))
      case Json.Constant.Schema(node)                => constant.decode(node.self, json)
      case Json.Dictionary.Schema(node)              => obj(json).map(_.toList).andThen(dictionary.decode(node.self, _))
      case Json.Enumeration.Schema(node)             => enumeration.decode(node.self, json)
      case Json.Optional.Schema(node)                => optional.decode(node.self, json)
      case schema @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveCirceDecoder.decode(schema, json)
      case schema @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveCirceDecoder.decode(schema, json)
      case schema @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveCirceDecoder.decode(schema, json)
      case Json.Record.Schema(node)                  =>
        obj(json).map(values => Fields.from(values.toIterable)).andThen(record.decode(node.self, _))
      case Json.Tuple.Schema(node) =>
        array(json).andThen(values => tuple.decode(node.self, values.toVector))
      case Json.Union.Schema(node) => union.decode(node.self, json)

  private def mismatch(name: String, json: CirceJson): Violations =
    Violations(
      Violation(constraint = Constraint.Generic.Type(name), actual = JsonCirce.typeOf(json).asData, hint = none)
    )

  private def array(json: CirceJson): Validated[Violations, Vector[CirceJson]] =
    json.asArray.toValid(mismatch("array", json))

  private def obj(json: CirceJson): Validated[Violations, io.circe.JsonObject] =
    json.asObject.toValid(mismatch("object", json))
