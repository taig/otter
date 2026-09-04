package io.taig.otter.codec

import io.taig.otter.Json

object JsonBorerEncoder extends Encoder[Json.Node, BorerWrite]:
  private val coerce: CoerceEncoder[Json.Primitive.Node, BorerWrite] = CoerceEncoder(JsonPrimitiveBorerEncoder)

  private val collection: CollectionEncoder[Json.Node, BorerWrite, BorerWrite] = CollectionEncoder(this, identity)

  private val constant: ConstantEncoder[Json.Primitive.Node, BorerWrite] = ConstantEncoder(JsonPrimitiveBorerEncoder)

  private val dictionary: DictionaryEncoder[Json.Primitive.Text.Node, Json.Node, BorerWrite, BorerWrite] =
    DictionaryEncoder(JsonTextEncoder, this, JsonBorerEncoder.member)

  private val enumeration: EnumerationEncoder[Json.Primitive.Node, BorerWrite] =
    EnumerationEncoder(JsonPrimitiveBorerEncoder)

  private val optional: OptionalEncoder[Json.Node, BorerWrite] =
    OptionalEncoder(this, empty = BorerWrite(_.writeNull()))

  private val tuple: TupleEncoder[Json.Node, BorerWrite, BorerWrite] =
    TupleEncoder(this, empty = BorerWrite(_.writeNull()), identity)

  /** Lazy, unlike its neighbours, because [[JsonFieldBorerEncoder]] holds an encoder of this object: a `val` here would
    * have the two initialising each other. The same goes for [[JsonBranchBorerEncoder]] below.
    */
  private lazy val record: RecordEncoder[Json.Field.Node, BorerWrite] = RecordEncoder(JsonFieldBorerEncoder)

  private lazy val union: UnionEncoder[Json.Branch.Node, BorerWrite] = UnionEncoder(JsonBranchBorerEncoder)

  override def encode[W](json: Json.Node[W, Any], w: W): BorerWrite = json match
    case Json.Coerce.Schema(node)                => coerce.encode(node.self, w)
    case Json.Collection.Schema(node)            => JsonBorerEncoder.array(collection.encode(node.self, w))
    case Json.Constant.Schema(node)              => constant.encode(node.self, w)
    case Json.Dictionary.Schema(node)            => JsonBorerEncoder.obj(dictionary.encode(node.self, w))
    case Json.Enumeration.Schema(node)           => enumeration.encode(node.self, w)
    case Json.Optional.Schema(node)              => optional.encode(node.self, w)
    case json @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveBorerEncoder.encode(json, w)
    case json @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveBorerEncoder.encode(json, w)
    case json @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveBorerEncoder.encode(json, w)
    case Json.Record.Schema(node)                => JsonBorerEncoder.obj(record.encode(node.self, w))
    case Json.Tuple.Schema(node)                 => JsonBorerEncoder.array(tuple.encode(node.self, w))
    case Json.Union.Schema(node)                 => union.encode(node.self, w)

  /** Unsized, which is what borer's own JSON parser emits and what `Dom.elementEncoder` writes. A sized header would
    * need the member count up front, and a `Monoid` is not a collection to ask the length of.
    */
  private def array(elements: BorerWrite): BorerWrite =
    BorerWrite(writer => elements.write(writer.writeArrayStart()).writeBreak())

  private def obj(members: BorerWrite): BorerWrite =
    BorerWrite(writer => members.write(writer.writeMapStart()).writeBreak())

  private def member(name: String, value: BorerWrite): BorerWrite =
    BorerWrite(writer => value.write(writer.writeString(name)))
