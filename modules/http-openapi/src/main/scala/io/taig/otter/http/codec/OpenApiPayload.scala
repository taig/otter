package io.taig.otter.http.codec

import io.taig.otter.Json
import io.taig.otter.JsonSchemaDocument
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Side
import io.taig.otter.codec.JsonSchemaRenderer
import io.taig.otter.http.OpenApi

import scala.compiletime.asMatchable

/** What the document inside a body renders as, for whichever alphabets an API's bodies are written in.
  *
  * Open on purpose, and dispatching at runtime on purpose. A body's payload is deliberately unbounded -- that is what
  * [[io.taig.otter.http.Body.Payload]] says -- so at the point a renderer holds one, its alphabet is existential and
  * there is nothing left to dispatch on statically. A typed `Renderer[S, JsonSchemaDocument]` would work only for an
  * API whose every body is written in one alphabet, and would stop compiling the moment two were mixed, which is the
  * case the payload type was made open for.
  *
  * The trade is bounded by what happens when it is wrong: an alphabet nothing here recognises is reported as
  * [[io.taig.otter.http.OpenApiIssue.Undescribed]] and the body is still listed with its media type. Nothing throws,
  * and nothing is silently omitted.
  *
  * A [[io.taig.otter.http.Multipart]] payload is not handled here. It is not a document language but a structure of
  * bodies, so it belongs to the renderer that already knows how to render a body.
  */
trait OpenApiPayload:
  def render(side: Side, payload: Any): Option[JsonSchemaDocument]

  /** The name this payload asks to be declared under, if it asks for one.
    *
    * Asked for separately because a JSON Schema renderer puts a named *root* back inline -- correct for a standalone
    * document, where a consumer wants a shape and not a pointer to one -- and an OpenAPI document wants the opposite: a
    * schema several operations share is declared once under `components/schemas` and referred to from each of them.
    */
  def name(payload: Any): Option[String]

  /** This renderer, falling back to `that` for an alphabet it does not recognise. */
  final def orElse(that: OpenApiPayload): OpenApiPayload = new OpenApiPayload:
    override def render(side: Side, payload: Any): Option[JsonSchemaDocument] =
      OpenApiPayload.this.render(side, payload).orElse(that.render(side, payload))

    override def name(payload: Any): Option[String] =
      OpenApiPayload.this.name(payload).orElse(that.name(payload))

object OpenApiPayload:
  /** JSON documents, which is the alphabet every API has at least one body in. */
  def json(profile: JsonSchemaProfile): OpenApiPayload = new OpenApiPayload:
    override def render(side: Side, payload: Any): Option[JsonSchemaDocument] =
      payload.asMatchable match
        case json: Json.Schema[?, ?, ?] @unchecked =>
          val renderer = side match
            case Side.Read  => JsonSchemaRenderer.reader(profile)
            case Side.Write => JsonSchemaRenderer.writer(profile)

          Some(renderer.render[Nothing, Any](json))
        case _ => None

    override def name(payload: Any): Option[String] =
      payload.asMatchable match
        case json: Json.Schema[?, ?, ?] @unchecked => Json.name(OpenApi.Namespaces, json)
        case _                                     => None

  /** Recognising nothing, which is what an API with no described bodies needs. */
  val Empty: OpenApiPayload = new OpenApiPayload:
    override def render(side: Side, payload: Any): Option[JsonSchemaDocument] = None

    override def name(payload: Any): Option[String] = None
