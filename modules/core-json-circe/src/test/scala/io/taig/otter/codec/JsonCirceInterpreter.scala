package io.taig.otter.codec

import cats.data.Validated
import io.circe.parser
import io.taig.otter.Json
import io.taig.otter.Violations

object JsonCirceInterpreter extends JsonInterpreter:
  override val name: String = "JsonCirce"

  /** Through circe's own parser, which is what a caller of this module hands a document to. A document the parser
    * refuses is a broken fixture rather than something the schema has an answer for, so it fails the test outright
    * rather than being reported as a violation.
    */
  override def decode[A](schema: Json.Reader[A], document: String): Validated[Violations, A] =
    parser.parse(document) match
      case Left(failure) => sys.error(s"not a JSON document: $document ($failure)")
      case Right(json)   => JsonCirceDecoder.decode(schema, json)

  override def encode[A](schema: Json.Writer[A], value: A): String =
    JsonCirceEncoder.encode(schema, value).noSpaces

  /** Through the document, which is what this module has: the encoder builds an `io.circe.Json` and the decoder reads
    * one, so there is nothing to be gained by printing it and parsing it back.
    */
  override def roundTrip[A](schema: Json[A], value: A): Validated[Violations, A] =
    JsonCirceDecoder.decode(schema, JsonCirceEncoder.encode(schema, value))
