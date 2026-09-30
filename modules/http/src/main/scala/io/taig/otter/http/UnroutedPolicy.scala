package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.Union

/** Statically declared answers for a request no endpoint is addressed to.
  *
  * Write only, and with no error type, because nothing reads these answers back. A client calling an endpoint never
  * receives one unless it and the server disagree about what exists, and an OpenAPI document has nowhere to put a
  * response that belongs to no operation. Tying them to the API's `E` would therefore only widen what every client
  * returns by a case no client decodes, and would make a bodyless default impossible for an `E` that is not a
  * [[Status]].
  *
  * A response writer is contravariant in what it writes, so one writer at [[Unrouted]] fills both fields.
  */
final case class UnroutedPolicy[+S[-_, +_]](
    notFound: Response.Writer.Of[S, Unrouted.NotFound],
    methodNotAllowed: Response.Writer.Of[S, Unrouted.MethodNotAllowed]
):
  /** Selection is encoded into the schema, as it is for [[ErrorPolicy.responses]]. */
  val responses: Responses.Writer.Of[S, Unrouted] =
    val left: Union[Response.Schema[S, *, *], Unrouted.NotFound, Any] = Union.Root(Reference.now(notFound))
    val right: Union[Response.Schema[S, *, *], Unrouted.MethodNotAllowed, Any] =
      Union.Root(Reference.now(methodNotAllowed))

    Responses.Schema(
      Union.Modify(
        Union.Coproduct(left, right),
        _.fold(identity, identity),
        (unrouted: Unrouted) =>
          unrouted match
            case unrouted: Unrouted.NotFound         => Left(unrouted)
            case unrouted: Unrouted.MethodNotAllowed => Right(unrouted)
      )
    )

object UnroutedPolicy:
  /** Bodyless answers, which work with every payload interpreter. */
  val default: UnroutedPolicy[Nothing] =
    def response[A](status: Int): Response.Writer.Of[Nothing, A] =
      Response.Schema(Response.Value.Modify(Response.Value.Root(Status(status)), _ => (), (_: A) => ()))

    UnroutedPolicy(response(404), response(405))
