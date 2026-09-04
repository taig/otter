package io.taig.otter.codec

import cats.data.Validated
import io.bullet.borer.Dom
import io.bullet.borer.Json as BorerJson
import io.taig.otter.Json
import io.taig.otter.JsonBorer
import io.taig.otter.Violations

import java.nio.charset.StandardCharsets.UTF_8

object JsonBorerInterpreter extends JsonInterpreter:
  override val name: String = "JsonBorer"

  /** Through borer's own parser, which is the only thing this hands borer: bytes. A document it refuses throws, which
    * is what a broken fixture deserves.
    */
  override def decode[A](schema: Json.Reader[A], document: String): Validated[Violations, A] =
    JsonBorerDecoder.decode(schema, BorerJson.decode(document.getBytes(UTF_8)).to[Dom.Element].value)

  override def encode[A](schema: Json.Writer[A], value: A): String =
    BorerJson.encode(value)(using JsonBorer.encoder(schema)).toUtf8String

  /** Through bytes, which is the whole point of the module: the encoder never builds a document, so the wire is the
    * only meeting place there is.
    */
  override def roundTrip[A](schema: Json[A], value: A): Validated[Violations, A] =
    BorerJson
      .decode(BorerJson.encode(value)(using JsonBorer.encoder(schema)).toByteArray)
      .to(using JsonBorer.validated(schema))
      .value
