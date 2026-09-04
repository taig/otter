package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Dom
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.Violations
import io.taig.validation.Violation

object JsonBorerDecoder extends Decoder[Json.Node, Dom.Element]:
  private val collection: CollectionDecoder[Json.Node, Dom.Element] = CollectionDecoder(this)

  private val constant: ConstantDecoder[Json.Primitive.Node, Dom.Element] =
    ConstantDecoder(JsonPrimitiveBorerDecoder, JsonPrimitiveBorerDomEncoder, JsonBorer.toData)

  private val dictionary: DictionaryDecoder[Json.Primitive.Text.Node, Json.Node, Dom.Element] =
    DictionaryDecoder(JsonTextDecoder, this)

  private val enumeration: EnumerationDecoder[Json.Primitive.Node, Dom.Element] =
    EnumerationDecoder(JsonPrimitiveBorerDecoder, JsonPrimitiveBorerDomEncoder, JsonBorer.toData)

  private val optional: OptionalDecoder[Json.Node, Dom.Element] = OptionalDecoder(this, empty = _ == Dom.NullElem)

  private val tuple: TupleDecoder[Json.Node, Dom.Element] = TupleDecoder(this, empty = _ == Dom.NullElem)

  /** Lazy, unlike its neighbours, because [[JsonFieldBorerDecoder]] holds a decoder of this object: a `val` here would
    * have the two initialising each other. The same goes for [[JsonBranchBorerDecoder]] below.
    */
  private lazy val record: RecordDecoder[Json.Field.Node, Dom.Element] = RecordDecoder(JsonFieldBorerDecoder)

  private lazy val union: UnionDecoder[Json.Branch.Node, Dom.Element] = UnionDecoder(JsonBranchBorerDecoder)

  override def decode[R](schema: Json.Node[Nothing, R], element: Dom.Element): Validated[Violations, R] =
    schema match
      case Json.Coerce.Schema(node)     => JsonCoerceBorerDecoder.decode(node.self, element)
      case Json.Collection.Schema(node) =>
        JsonBorerDecoder.array(element).andThen(collection.decode(node.self, _))
      case Json.Constant.Schema(node)   => constant.decode(node.self, element)
      case Json.Dictionary.Schema(node) =>
        JsonBorerDecoder.members(element).andThen(dictionary.decode(node.self, _))
      case Json.Enumeration.Schema(node)             => enumeration.decode(node.self, element)
      case Json.Optional.Schema(node)                => optional.decode(node.self, element)
      case schema @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveBorerDecoder.decode(schema, element)
      case schema @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveBorerDecoder.decode(schema, element)
      case schema @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveBorerDecoder.decode(schema, element)
      case Json.Record.Schema(node)                  =>
        JsonBorerDecoder.members(element).map(Fields.from).andThen(record.decode(node.self, _))
      case Json.Tuple.Schema(node) => JsonBorerDecoder.array(element).andThen(tuple.decode(node.self, _))
      case Json.Union.Schema(node) => union.decode(node.self, element)

  private def mismatch(name: String, element: Dom.Element): Violations =
    Violations(
      Violation(constraint = Constraint.Generic.Type(name), actual = JsonBorer.typeOf(element).asData, hint = none)
    )

  /** Both array elements, because borer's JSON parser emits the unsized form and CBOR the sized one. */
  private def array(element: Dom.Element): Validated[Violations, Vector[Dom.Element]] = element match
    case element: Dom.ArrayElem => element.elems.valid
    case _                      => JsonBorerDecoder.mismatch("array", element).invalid

  /** An object's members in arrival order, duplicates and all, which is what [[Fields]] and a dictionary both want.
    *
    * `stringKeyedMembers` *drops* a member whose key is not a string, which no document borer parsed can hold but a
    * `Dom` built by hand can, and silently losing a member is a worse answer than reporting it. Counting what came back
    * against the map's own size catches that in one pass, where traversing the members through a `Validated` allocated
    * one per member to say what is true of all but a hand built few.
    */
  private def members(element: Dom.Element): Validated[Violations, List[(String, Dom.Element)]] = element match
    case element: Dom.MapElem =>
      val members = element.stringKeyedMembers.toList

      if members.length == element.size then members.valid else JsonBorerDecoder.key(element).invalid
    case _ => JsonBorerDecoder.mismatch("object", element).invalid

  /** Only ever on the failure path: a member was dropped, so find the key that dropped it and report that. */
  private def key(element: Dom.MapElem): Violations =
    val offending = element.keys.find:
      case _: Dom.AbstractTextElem => false
      case _                       => true

    JsonBorerDecoder.mismatch("string", offending.getOrElse(element))
