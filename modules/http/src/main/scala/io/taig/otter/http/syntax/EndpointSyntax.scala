package io.taig.otter.http.syntax

import io.taig.otter as Self
import io.taig.otter.Append
import io.taig.otter.Reference
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Headers
import io.taig.otter.http.Queries
import io.taig.otter.http.Request
import io.taig.otter.http.Result

/** Adding a part to a request or a result.
  *
  * Extension methods rather than members, for the reason [[io.taig.otter.syntax.OtterSyntax]]'s `:*` is one: appending
  * needs an [[Append.Shape]] for what the schema already holds, and [[Append.Shape]] is invariant, so a contravariant
  * class parameter cannot appear in it. As extensions, what the schema holds is a method parameter and the constraint
  * is expressible.
  *
  * Each step drops a `Unit`, exactly as `:*` does, so `request(Method.Get, path).headers(HNil)` holds what the path
  * holds and nothing more, and only the parts that carry something reach the caller.
  */
trait EndpointSyntax:
  extension [S[-w, +r], W1, R1](fa: Request.Schema[S, W1, R1])
    /** The query string this request reads. */
    def queries[W2, R2](values: => Queries.Node[W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Request.Schema[S, Append[W1, W2], Append[R1, R2]] =
      Request.Schema(
        Request.Value.Modify(
          Request.Value.Queries(fa.self.self, Reference.later(values)),
          (values: (R1, R2)) => R.join(values._1, values._2),
          W.split
        )
      )

    /** The headers this request reads. */
    def headers[W2, R2](values: => Headers.Node[W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Request.Schema[S, Append[W1, W2], Append[R1, R2]] =
      Request.Schema(
        Request.Value.Modify(
          Request.Value.Headers(fa.self.self, Reference.later(values)),
          (values: (R1, R2)) => R.join(values._1, values._2),
          W.split
        )
      )

  /** A request that carries no body yet.
    *
    * Pinning the payload to `Nothing` is how "at most one entity" becomes a compile error rather than a rule: a request
    * that already has a body is not one of these, so `.body(a).body(b)` does not typecheck. It is also why nothing here
    * unions payload types -- there is only ever one -- which keeps `Nothing` out of the unions the earlier shape
    * accumulated for no reason.
    */
  extension [W1, R1](fa: Request.Schema[Nothing, W1, R1])
    /** The one body this request carries. */
    def body[S2[-w, +r], W2, R2](value: => Body.Schema[S2, W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Request.Schema[S2, Append[W1, W2], Append[R1, R2]] =
      fa.bodies(Bodies.Schema.apply[S2, W2, R2](Self.Union.Root(Reference.later(value))))

    /** The body this request carries, as a choice between alternatives. */
    def bodies[S2[-w, +r], W2, R2](values: => Bodies.Schema[S2, W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Request.Schema[S2, Append[W1, W2], Append[R1, R2]] =
      Request.Schema(
        Request.Value.Modify(
          Request.Value.Payload[S2, W1, R1, W2, R2](fa.self.self, Reference.later(values)),
          (values: (R1, R2)) => R.join(values._1, values._2),
          W.split
        )
      )

    /** The streamed body this request carries, which changes what it describes and not what it holds. */
    def streaming[S2[-w, +r], W2, R2](value: => Body.Streamed.Schema[S2, W2, R2]): Request.Schema[S2, W1, R1] =
      Request.Schema(Request.Value.Streaming[S2, W1, R1, W2, R2](fa.self.self, Reference.later(value)))

  extension [S[-w, +r], W1, R1](fa: Result.Schema[S, W1, R1])
    /** The headers this result writes. */
    def headers[W2, R2](values: => Headers.Node[W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Result.Schema[S, Append[W1, W2], Append[R1, R2]] =
      Result.Schema(
        Result.Value.Modify(
          Result.Value.Headers(fa.self.self, Reference.later(values)),
          (values: (R1, R2)) => R.join(values._1, values._2),
          W.split
        )
      )

  /** A result that carries no body yet, for the reason the request counterpart is pinned the same way. */
  extension [W1, R1](fa: Result.Schema[Nothing, W1, R1])
    /** The one body this result carries. */
    def body[S2[-w, +r], W2, R2](value: => Body.Schema[S2, W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Result.Schema[S2, Append[W1, W2], Append[R1, R2]] =
      fa.bodies(Bodies.Schema.apply[S2, W2, R2](Self.Union.Root(Reference.later(value))))

    /** The body this result carries, as a choice between alternatives. */
    def bodies[S2[-w, +r], W2, R2](values: => Bodies.Schema[S2, W2, R2])(using
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): Result.Schema[S2, Append[W1, W2], Append[R1, R2]] =
      Result.Schema(
        Result.Value.Modify(
          Result.Value.Payload[S2, W1, R1, W2, R2](fa.self.self, Reference.later(values)),
          (values: (R1, R2)) => R.join(values._1, values._2),
          W.split
        )
      )

    /** The streamed body this result carries, which changes what it describes and not what it holds. */
    def streaming[S2[-w, +r], W2, R2](value: => Body.Streamed.Schema[S2, W2, R2]): Result.Schema[S2, W1, R1] =
      Result.Schema(Result.Value.Streaming[S2, W1, R1, W2, R2](fa.self.self, Reference.later(value)))

object EndpointSyntax extends EndpointSyntax
