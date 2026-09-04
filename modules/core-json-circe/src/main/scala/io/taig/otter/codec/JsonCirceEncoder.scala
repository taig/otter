package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Node, CirceJson]:
  private val coerce: CoerceEncoder[Json.Primitive.Node, CirceJson] = CoerceEncoder(JsonPrimitiveCirceEncoder)

  private val collection: CollectionEncoder[Json.Node, CirceJson, Vector[CirceJson]] = CollectionEncoder(this)

  private val constant: ConstantEncoder[Json.Primitive.Node, CirceJson] = ConstantEncoder(JsonPrimitiveCirceEncoder)

  private val dictionary: DictionaryEncoder[Json.Primitive.Text.Node, Json.Node, CirceJson, List[(String, CirceJson)]] =
    DictionaryEncoder(JsonTextEncoder, this)

  private val enumeration: EnumerationEncoder[Json.Primitive.Node, CirceJson] =
    EnumerationEncoder(JsonPrimitiveCirceEncoder)

  private val optional: OptionalEncoder[Json.Node, CirceJson] = OptionalEncoder(this, empty = CirceJson.Null)

  private val tuple: TupleEncoder[Json.Node, CirceJson, Vector[CirceJson]] =
    TupleEncoder(this, empty = CirceJson.Null)

  /** Lazy, unlike its neighbours, because [[JsonFieldCirceEncoder]] holds an encoder of this object: a `val` here would
    * have the two initialising each other. The same goes for [[JsonBranchCirceEncoder]] below.
    */
  private lazy val record: RecordEncoder[Json.Field.Node, Chain[(String, CirceJson)]] =
    RecordEncoder(JsonFieldCirceEncoder)

  private lazy val union: UnionEncoder[Json.Branch.Node, CirceJson] = UnionEncoder(JsonBranchCirceEncoder)

  override def encode[W](json: Json.Node[W, Any], w: W): CirceJson = json match
    case Json.Coerce.Schema(node)                => coerce.encode(node.self, w)
    case Json.Collection.Schema(node)            => CirceJson.fromValues(collection.encode(node.self, w))
    case Json.Constant.Schema(node)              => constant.encode(node.self, w)
    case Json.Dictionary.Schema(node)            => CirceJson.fromFields(dictionary.encode(node.self, w))
    case Json.Enumeration.Schema(node)           => enumeration.encode(node.self, w)
    case Json.Optional.Schema(node)              => optional.encode(node.self, w)
    case json @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveCirceEncoder.encode(json, w)
    case json @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveCirceEncoder.encode(json, w)
    case json @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveCirceEncoder.encode(json, w)
    case Json.Record.Schema(node)                => CirceJson.fromFields(record.encode(node.self, w).toList)
    case Json.Tuple.Schema(node)                 => CirceJson.fromValues(tuple.encode(node.self, w))
    case Json.Union.Schema(node)                 => union.encode(node.self, w)
