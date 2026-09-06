package io.taig.otter.http.codec

import cats.data.State
import io.taig.otter.Typescript
import io.taig.otter.codec.JsonTypescriptContext

/** What the document inside a body is generated as, for whichever alphabets an API's bodies are written in.
  *
  * [[io.taig.otter.http.codec.OpenApiPayload]] again, and open and runtime dispatched for the same reason: a body's
  * payload is deliberately unbounded, so at the point a renderer holds one its alphabet is existential and there is
  * nothing left to dispatch on statically. An alphabet nothing here recognises is reported as
  * [[io.taig.otter.http.TypescriptIssue.Undescribed]] and the body is still described by its media type. Nothing throws
  * and nothing is silently omitted.
  *
  * The result is a `State` over [[JsonTypescriptContext]] rather than a bare expression, which is what lets every
  * payload of every endpoint share one set of declarations: a schema two endpoints send is declared once, and named the
  * second time it is reached.
  *
  * A [[io.taig.otter.http.Multipart]] payload is not handled here. It is not a document language but a structure of
  * bodies, so it belongs to the renderer that already knows what a body is.
  */
trait TypescriptPayload:
  def render(side: io.taig.otter.Side, payload: Any): Option[State[JsonTypescriptContext, Typescript.Expression]]

  /** The name this payload asks to be declared under, if it asks for one. */
  def name(payload: Any): Option[String]

  /** This renderer, falling back to `that` for an alphabet it does not recognise. */
  final def orElse(that: TypescriptPayload): TypescriptPayload = new TypescriptPayload:
    override def render(
        side: io.taig.otter.Side,
        payload: Any
    ): Option[State[JsonTypescriptContext, Typescript.Expression]] =
      TypescriptPayload.this.render(side, payload).orElse(that.render(side, payload))

    override def name(payload: Any): Option[String] =
      TypescriptPayload.this.name(payload).orElse(that.name(payload))

object TypescriptPayload:
  /** Recognising nothing, which is what an API with no described bodies needs. */
  val Empty: TypescriptPayload = new TypescriptPayload:
    override def render(
        side: io.taig.otter.Side,
        payload: Any
    ): Option[State[JsonTypescriptContext, Typescript.Expression]] = None

    override def name(payload: Any): Option[String] = None
