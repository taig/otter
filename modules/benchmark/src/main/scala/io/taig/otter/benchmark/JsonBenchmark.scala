package io.taig.otter.benchmark

import io.bullet.borer.Dom
import io.bullet.borer.Json as BorerJson
import io.circe.Json as CirceJson
import io.circe.parser
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.codec.JsonBorerDecoder
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import org.openjdk.jmh.annotations.*

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit

/** What every JSON benchmark measures, given a schema and one value it round trips.
  *
  * Reading is measured three ways: from a document, which is the schema interpreter on its own; from text, which is
  * that interpreter behind circe's parser; and `parseText`, which is the parser with no schema in sight. The three
  * together say how much of a read is the intermediate tree and how much is walking the schema.
  *
  * Writing needs three the same way, and for a while had only two. `encodeDocument` is not the counterpart of
  * `parseText`: it walks the schema *and* builds the tree, so subtracting `printDocument` from `encodeText` leaves the
  * two of them added together and says nothing about either. `encodeVoid` is the missing third -- the whole walk with
  * nothing built -- so the document model's share of a write is `encodeDocument - encodeVoid`, and the share that is
  * otter's own interpreter is `encodeVoid` itself.
  *
  * Read a result against `encodeDocument - encodeVoid` and `printDocument` on writes, and against `parseText` on reads.
  * Those are the whole of what swapping in a JSON library without a document model could win; the rest is the schema
  * interpreter, which such a library would not touch.
  */
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
abstract class JsonBenchmark[A](schema: Json[A], value: A):
  private val text: String = JsonCirceEncoder.encode(schema, value).noSpaces

  /** Parsed rather than encoded, and the difference is not cosmetic.
    *
    * circe has two `JsonObject` implementations and they do different work at the one line the record decoder calls.
    * `JsonCirceDecoder` reads a record through `Fields.from(values.toIterable)`; the parser's
    * `LinkedHashMapJsonObject.toIterable` is an anonymous `Iterable` over the map's iterator, so `Vector.from` runs a
    * full builder pass over it, while the encoder's `MapAndVectorJsonObject.toIterable` is already a `Vector` and is
    * returned as it stands. A `decodeDocument` over an encoder built document therefore measures work no read does, and
    * `decodeText - parseText` does not separate parsing from interpreting.
    */
  private val document: CirceJson = parser.parse(text).getOrElse(sys.error(s"unparsable fixture: $text"))

  private val bytes: Array[Byte] = text.getBytes(UTF_8)

  private val element: Dom.Element = BorerJson.decode(bytes).to[Dom.Element].value

  private val borerDecoder: io.bullet.borer.Decoder[A] = JsonBorer.decoder(schema)

  private val borerEncoder: io.bullet.borer.Encoder[A] = JsonBorer.encoder(schema)

  @Benchmark
  def decodeDocument: Any = JsonCirceDecoder.decode(schema, document)

  @Benchmark
  def decodeText: Any = parser.parse(text).map(JsonCirceDecoder.decode(schema, _))

  @Benchmark
  def encodeDocument: Any = JsonCirceEncoder.encode(schema, value)

  @Benchmark
  def encodeText: Any = JsonCirceEncoder.encode(schema, value).noSpaces

  /** The schema walk of a write with nothing written, which is what `encodeDocument` is measured against to leave the
    * document model on its own. See [[JsonVoidEncoder]].
    */
  @Benchmark
  def encodeVoid: Any = JsonVoidEncoder.encode(schema, value)

  /** The document model with no schema in sight: what it costs to build a tree out of the text, and to print one back
    * out. `parseText` is a read's whole document cost; `printDocument` is only half of a write's, the other half being
    * `encodeDocument - encodeVoid`.
    */
  @Benchmark
  def parseText: Any = parser.parse(text)

  @Benchmark
  def printDocument: Any = document.noSpaces

  /** What `encodeBorerBytes` is actually competing with: the bytes a caller sends, not the `String` circe stops at. */
  @Benchmark
  def encodeTextBytes: Any = JsonCirceEncoder.encode(schema, value).noSpaces.getBytes(UTF_8)

  @Benchmark
  def decodeBorerDocument: Any = JsonBorerDecoder.decode(schema, element)

  @Benchmark
  def decodeBorerBytes: Any = BorerJson.decode(bytes).to(using borerDecoder).valueEither

  @Benchmark
  def encodeBorerBytes: Any = BorerJson.encode(value)(using borerEncoder).toByteArray

  /** borer's document model on its own, the counterpart of `parseText` and `printDocument`. `encodeBorerBytes` goes
    * through neither of these, which is the whole point of the module.
    */
  @Benchmark
  def parseBorerBytes: Any = BorerJson.decode(bytes).to[Dom.Element].valueEither

  @Benchmark
  def printBorerDocument: Any = BorerJson.encode(element).toByteArray
