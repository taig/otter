package io.taig.otter.benchmark

import io.taig.otter.Absence
import io.taig.otter.Json
import io.taig.otter.Primitive
import io.taig.otter.codec.BranchEncoder
import io.taig.otter.codec.CoerceEncoder
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.JsonTextEncoder
import io.taig.otter.codec.OptionalEncoder
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.codec.UnionEncoder

/** The schema walk of a write, with nothing written.
  *
  * `encodeDocument` measures two things at once: walking the schema, and building the document out of what the walk
  * finds. Nothing in the benchmark separated them, so the write side had no counterpart to `parseText` and the cost of
  * a document model on writes could only be inferred. This is that counterpart: it takes every one of the twelve cases
  * apart, splits the tuples, forces the `Eval`s, reads the annotations and combines what the combinators return -- all
  * of it at `T = Unit`, so the only thing missing is the document.
  *
  * `encodeDocument` minus `encodeVoid` is therefore the document model's share of a write, and it is the budget a JSON
  * library without one is competing for. Reading a `Text.Format`'s `print` is on this side of the line rather than the
  * other: turning a `UUID` into text is the schema's own business, not the document's.
  */
object JsonVoidEncoder extends Encoder[Json.Node, Unit]:
  private val collection: CollectionEncoder[Json.Node, Unit, Unit] = CollectionEncoder(this, _ => ())

  private val dictionary: DictionaryEncoder[Json.Primitive.Text.Node, Json.Node, Unit, Unit] =
    DictionaryEncoder(JsonTextEncoder, this, (_, _) => ())

  private val optional: OptionalEncoder[Json.Node, Unit] = OptionalEncoder(this, empty = ())

  private val tuple: TupleEncoder[Json.Node, Unit, Unit] = TupleEncoder(this, empty = (), _ => ())

  private lazy val record: RecordEncoder[Json.Field.Node, Unit] = RecordEncoder(JsonFieldVoidEncoder)

  private lazy val union: UnionEncoder[Json.Branch.Node, Unit] = UnionEncoder(JsonBranchVoidEncoder)

  override def encode[W](json: Json.Node[W, Any], w: W): Unit = json match
    case Json.Coerce.Schema(node)                => CoerceEncoder(JsonPrimitiveVoidEncoder).encode(node.self, w)
    case Json.Collection.Schema(node)            => collection.encode(node.self, w)
    case Json.Constant.Schema(node)              => ConstantEncoder(JsonPrimitiveVoidEncoder).encode(node.self, w)
    case Json.Dictionary.Schema(node)            => dictionary.encode(node.self, w)
    case Json.Enumeration.Schema(node)           => EnumerationEncoder(JsonPrimitiveVoidEncoder).encode(node.self, w)
    case Json.Optional.Schema(node)              => optional.encode(node.self, w)
    case json @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveVoidEncoder.encode(json, w)
    case json @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveVoidEncoder.encode(json, w)
    case json @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveVoidEncoder.encode(json, w)
    case Json.Record.Schema(node)                => record.encode(node.self, w)
    case Json.Tuple.Schema(node)                 => tuple.encode(node.self, w)
    case Json.Union.Schema(node)                 => union.encode(node.self, w)

/** The primitive walk of a write, with nothing written. `print` is called and discarded: what a format turns a value
  * into is the schema's work, and only the document that would hold the result belongs to the library.
  */
object JsonPrimitiveVoidEncoder extends Encoder[Json.Primitive.Node, Unit]:
  override def encode[W](json: Json.Primitive.Node[W, Any], w: W): Unit = json match
    case Json.Primitive.Boolean.Schema(annotation) => encode(annotation.self, w)
    case Json.Primitive.Number.Schema(annotation)  => encode(annotation.self, w)
    case Json.Primitive.Text.Schema(annotation)    => encode(annotation.self, w)

  def encode[W](schema: Primitive[W, Any], w: W): Unit = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => ()
    case Primitive.Number.BigDecimal(_)       => ()
    case Primitive.Number.BigInteger(_)       => ()
    case Primitive.Number.Double(_)           => ()
    case Primitive.Number.Float(_)            => ()
    case Primitive.Number.Int(_)              => ()
    case Primitive.Number.Long(_)             => ()
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => val _ = print(w)
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => ()

/** Reads the annotation `JsonFieldCirceEncoder` reads, so that the metadata lookup is on the walk's side of the line
  * rather than the document's.
  */
object JsonFieldVoidEncoder extends Encoder[Json.Field.Node, Unit]:
  private val omitting: FieldEncoder[Json.Node, Unit, Unit] =
    FieldEncoder(JsonVoidEncoder, absent = None, (_, _) => ())

  private val nulling: FieldEncoder[Json.Node, Unit, Unit] =
    FieldEncoder(JsonVoidEncoder, absent = Some(()), (_, _) => ())

  override def encode[W](json: Json.Field.Node[W, Any], w: W): Unit =
    val encoder = Json.absence(json.self.metadata) match
      case Absence.Empty => nulling
      case Absence.Omit  => omitting

    encoder.encode(json.self.self, w)

val JsonBranchVoidEncoder: Encoder[Json.Branch.Node, Unit] =
  BranchEncoder(JsonVoidEncoder).contramapK([w, r] => (json: Json.Branch.Node[w, r]) => json.self.self)
