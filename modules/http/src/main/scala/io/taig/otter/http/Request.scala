package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Direction
import io.taig.otter.Metadata
import io.taig.otter.Reference

/** A request that round trips `A`. */
type Request[A] = Request.Of[Body.Payload, A]

object Request:
  /** A request holding the payload `S` and round tripping `A`.
    *
    * The parts are added one at a time -- `request(Method.Get, path).queries(q).headers(h).body(b)` -- and each step
    * appends to what the request holds, dropping the `Unit`s a part with nothing to say contributes. A request carries
    * at most one body and at most one streamed body, which is not a restriction the earlier attempts' shape imposed by
    * accident but the truth about HTTP: a request has one entity. What used to need several bodies -- a file beside its
    * metadata -- is one body whose payload is a [[Multipart]].
    */
  type Of[S[-w, +r], A] = Request.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Request.Schema[Body.Payload, w, r]

  /** What a server reads out of a request. */
  type Reader[+A] = Request.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Request.Schema[S, Nothing, A]

  /** What a client writes into one. */
  type Writer[-A] = Request.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Request.Schema[S, A, Any]

  final case class Schema[+S[-w, +r], -W, +R](self: Annotation[Request.Value[S, W, R]]):
    export self.self.{bodies, headers, method, path, queries, streamed}

  object Schema:
    def apply[S[-w, +r], W, R](self: Request.Value[S, W, R]): Request.Schema[S, W, R] =
      new Request.Schema(Annotation(self))

    given annotated: [S[-w, +r], W, R] => Annotated[Request.Schema[S, W, R]]:
      extension (self: Request.Schema[S, W, R])
        override def lens: (Metadata, Metadata => Request.Schema[S, W, R]) =
          (self.self.metadata, metadata => new Request.Schema(self.self.copy(metadata = metadata)))

    given profunctor: [S[-w, +r]] => Profunctor[[w, r] =>> Request.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Request.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Request.Schema[S, W, R] =
        new Request.Schema(self.self.map(Request.Value.Modify(_, g, f)))

    given functor: [S[-w, +r]] => Functor[[a] =>> Request.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Request.Schema[S, w, r]]

    given contravariant: [S[-w, +r]] => Contravariant[[a] =>> Request.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Request.Schema[S, w, r]]

    given invariant: [S[-w, +r]] => Invariant[[a] =>> Request.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Request.Schema[S, w, r]]

  /** What a request is made of.
    *
    * Every case but [[Request.Value.Root]] wraps another, so the accessors read down the chain and answer for the whole
    * request. A part that was never added answers `None`, which is what lets a renderer ask "is there a query string"
    * without the schema having to carry an empty one.
    */
  sealed abstract class Value[+S[-w, +r], -W, +R]:
    def method: Method

    def path: Reference[Path.Node, ?, ?]

    def queries: Option[Reference[Queries.Node, ?, ?]]

    def headers: Option[Reference[Headers.Node, ?, ?]]

    def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]]

    def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], ?, ?]]

  object Value:
    final case class Root[-W, +R](override val method: Method, override val path: Reference[Path.Node, W, R])
        extends Request.Value[Nothing, W, R]:
      override def queries: Option[Reference[io.taig.otter.http.Queries.Node, ?, ?]] = None

      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = None

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[Nothing, w, r], ?, ?]] = None

      override def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[Nothing, w, r], ?, ?]] = None

    final case class Queries[+S[-w, +r], W1, R1, W2, R2](
        self: Request.Value[S, W1, R1],
        values: Reference[io.taig.otter.http.Queries.Node, W2, R2]
    ) extends Request.Value[S, (W1, W2), (R1, R2)]:
      export self.{bodies, headers, method, path, streamed}

      override def queries: Option[Reference[io.taig.otter.http.Queries.Node, ?, ?]] = Some(values)

    final case class Headers[+S[-w, +r], W1, R1, W2, R2](
        self: Request.Value[S, W1, R1],
        values: Reference[io.taig.otter.http.Headers.Node, W2, R2]
    ) extends Request.Value[S, (W1, W2), (R1, R2)]:
      export self.{bodies, method, path, queries, streamed}

      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = Some(values)

    final case class Payload[+S[-w, +r], W1, R1, W2, R2](
        self: Request.Value[S, W1, R1],
        values: Reference[[w, r] =>> Bodies.Schema[S, w, r], W2, R2]
    ) extends Request.Value[S, (W1, W2), (R1, R2)]:
      export self.{headers, method, path, queries, streamed}

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]] = Some(values)

    /** A streamed body added to a request, which changes what the request describes without changing what it holds.
      *
      * `W1` and `R1` pass through untouched. That is the whole of the streaming decision made visible: the stream is
      * described, and what a sequence of its elements is stays with whoever has an effect type to say it in.
      */
    final case class Streaming[+S[-w, +r], W1, R1, W2, R2](
        self: Request.Value[S, W1, R1],
        value: Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], W2, R2]
    ) extends Request.Value[S, W1, R1]:
      export self.{bodies, headers, method, path, queries}

      override def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], ?, ?]] = Some(value)

    final case class Modify[+S[-w, +r], W0, R0, -W, +R](
        self: Request.Value[S, W0, R0],
        f: R0 => R,
        g: W => W0
    ) extends Request.Value[S, W, R]:
      export self.{bodies, headers, method, path, queries, streamed}
