package io.taig.otter.benchmark

import io.circe.Json as CirceJson
import io.circe.parser
import io.taig.otter.Json
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit

/** What every JSON benchmark measures, given a schema and one value it round trips.
  *
  * Reading is measured twice: from a document, which is the schema interpreter on its own, and from text, which is the
  * interpreter behind circe's parser. The two together say how much of a read is the intermediate tree and how much is
  * walking the schema, and that split is what decides whether a JSON library without a document model would pay for
  * itself here.
  *
  * Writing is measured the same way round: to a document, and to the text a document is printed as.
  */
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
@Fork(1)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
abstract class JsonBenchmark[A](schema: Json[A], value: A):
  private val document: CirceJson = JsonCirceEncoder.encode(schema, value)

  private val text: String = document.noSpaces

  @Benchmark
  def decodeDocument: Any = JsonCirceDecoder.decode(schema, document)

  @Benchmark
  def decodeText: Any = parser.parse(text).map(JsonCirceDecoder.decode(schema, _))

  @Benchmark
  def encodeDocument: Any = JsonCirceEncoder.encode(schema, value)

  @Benchmark
  def encodeText: Any = JsonCirceEncoder.encode(schema, value).noSpaces

  /** The document model on its own, with no schema in sight: what it costs to build a tree out of the text, and to
    * print one back out. This is the whole of what a JSON library without a document model has to offer, so it is the
    * budget any such library is competing for -- the rest of a read or a write is the schema interpreter, which such a
    * library would not touch.
    */
  @Benchmark
  def parseText: Any = parser.parse(text)

  @Benchmark
  def printDocument: Any = document.noSpaces
