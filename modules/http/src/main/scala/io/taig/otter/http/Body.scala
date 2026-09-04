package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Direction
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.operation.AlternableOperation
import io.taig.otter.operation.UnionableOperation
import scodec.bits.ByteVector

/** A body that round trips `A`. */
type Body[A] = Body.Of[Body.Payload, A]

object Body:
  /** A payload schema, whatever alphabet it is written in.
    *
    * Deliberately open. Every other tier of this module narrows its `S` to say what may appear inside it, and this one
    * is the opposite: a body's content is a document, and which language that document is written in is not HTTP's
    * business.
    *
    * `Any` rather than `Matchable`, though a renderer holding a payload does have to pattern match on it. Narrowing the
    * bound would be the tighter statement and costs more than it buys: a bound on a type constructor has to be written
    * applied, `Body.Or` accumulates payload alphabets through type parameters that carry no bound of their own, and the
    * two cannot both be had. The match happens through `asMatchable` at the one place that needs it instead. A JSON
    * schema, a CSV schema and a [[Multipart]] schema are all payloads, and [[io.taig.otter.Json.Or]]-style unions
    * accumulate them down a chain of alternatives, so an interpreter written for one alphabet accepts exactly the
    * bodies it can read by ordinary contravariance.
    */
  type Payload = [w, r] =>> Any

  /** No tier below this one carries a bound, and that is the statement: `S` is written `S[-w, +r]` with nothing above
    * it, because there is nothing every payload alphabet has in common to name. [[Body.Payload]] exists only to spell
    * "any of them" where a `Node` alias needs an argument -- it cannot be used as a bound, since an alias's own
    * parameters are invariant and `w` is not.
    */

  /** A body holding the payload `S` and round tripping `A`. */
  type Of[S[-w, +r], A] = Body.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Body.Schema[Body.Payload, w, r]

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:+` accumulates over alternatives. */
  type Or[S1[-w, +r], S2[-w, +r]] = [w, r] =>> S1[w, r] | S2[w, r]

  type Reader[+A] = Body.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Body.Schema[S, Nothing, A]

  type Writer[-A] = Body.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Body.Schema[S, A, Any]

  /** A body, with the metadata a renderer reads its description and examples from. */
  final case class Schema[+S[-w, +r], -W, +R](self: Annotation[Body.Value[S, W, R]]):
    def mediaType: MediaType = self.self.mediaType

  object Schema:
    def apply[S[-w, +r], W, R](self: Body.Value[S, W, R]): Body.Schema[S, W, R] =
      new Body.Schema(Annotation(self))

    given annotated: [S[-w, +r], W, R] => Annotated[Body.Schema[S, W, R]]:
      extension (self: Body.Schema[S, W, R])
        override def lens: (Metadata, Metadata => Body.Schema[S, W, R]) =
          (self.self.metadata, metadata => new Body.Schema(self.self.copy(metadata = metadata)))

    given profunctor: [S[-w, +r]] => Profunctor[[w, r] =>> Body.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Body.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Body.Schema[S, W, R] =
        new Body.Schema(self.self.map(Body.Value.profunctor.dimap(_)(f)(g)))

    given functor: [S[-w, +r]] => Functor[[a] =>> Body.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Body.Schema[S, w, r]]

    given contravariant: [S[-w, +r]] => Contravariant[[a] =>> Body.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Body.Schema[S, w, r]]

    given invariant: [S[-w, +r]] => Invariant[[a] =>> Body.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Body.Schema[S, w, r]]

    given unionable: [S[-w, +r]]
      => UnionableOperation[[w, r] =>> Body.Schema[S, w, r], [w, r] =>> Bodies.Schema[S, w, r]] =
      UnionableOperation.derived

    /** `body :+ body`. The result carries both children's payload, so the union accumulates down the chain. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          [w, r] =>> Body.Schema[S1, w, r],
          [w, r] =>> Bodies.Schema[Body.Or[S1, S2], w, r],
          [w, r] =>> Body.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Body.Schema[S1, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] =
        Bodies.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

      override def element[W, R](fb: => Body.Schema[S2, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] =
        Bodies.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  /** A streamed body, at the type of its element.
    *
    * [[Body.Schema]] round trips `Unit` for a streamed body, because that is what it contributes to the endpoint. This
    * carries the element type alongside, so that a backend can ask for the very body it is about to hand a stream for
    * and have the compiler check that the element is the one the endpoint described:
    *
    * {{{
    * def route[F[_], S[-_, +_], A, B, E](
    *     endpoint: Endpoint.Server[S, A, B],
    *     body: Body.Streamed.Schema[S, E, E],
    *     handler: (A, fs2.Stream[F, E]) => F[B]
    * ): Route[F, S, A, B]
    * }}}
    *
    * Without it the element type would be recoverable only from a comment. With it, the one thing left to check at
    * construction is that the body belongs to that endpoint, which is a value comparison and not a type.
    */
  object Streamed:
    type Of[S[-w, +r], A] = Body.Streamed.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Body.Streamed.Schema[Body.Payload, w, r]

    final case class Schema[+S[-w, +r], -W, +R](self: Annotation[Body.Value.Streamed[S, W, R]]):
      /** The same body as a [[Request]] sees it, which is as something contributing nothing. */
      def body: Body.Schema[S, Unit, Unit] = new Body.Schema(self)

      def frame: Frame = self.self.frame

      def mediaType: MediaType = self.self.mediaType

    object Schema:
      given annotated: [S[-w, +r], W, R] => Annotated[Body.Streamed.Schema[S, W, R]]:
        extension (self: Body.Streamed.Schema[S, W, R])
          override def lens: (Metadata, Metadata => Body.Streamed.Schema[S, W, R]) =
            (self.self.metadata, metadata => new Body.Streamed.Schema(self.self.copy(metadata = metadata)))

  /** What a body is: three forms, and the profunctor that maps them.
    *
    * A [[Body.Value.Whole]] is one document. A [[Body.Value.Binary]] is bytes with no document in them at all -- an
    * image, a PDF -- carried as a `ByteVector` so that comparing two of them means comparing their contents. A
    * [[Body.Value.Streamed]] is a sequence of documents arriving one at a time.
    */
  sealed abstract class Value[+S[-w, +r], -W, +R]:
    def mediaType: MediaType

  object Value:
    /** One document, read and written whole. */
    final case class Whole[+S[-w, +r], -W, +R](
        override val mediaType: MediaType,
        payload: Reference[S, W, R]
    ) extends Body.Value[S, W, R]

    /** Bytes, with no schema to describe them.
      *
      * `ByteVector` rather than `Array[Byte]`, which has reference equality and would make a body holding a literal
      * impossible to compare and a golden test impossible to write.
      */
    final case class Binary(override val mediaType: MediaType) extends Body.Value[Nothing, ByteVector, ByteVector]

    /** A sequence of documents, arriving one at a time.
      *
      * `W` and `R` are the *element*, and the body itself round trips `Unit` -- it contributes nothing to what the
      * endpoint holds. That is the whole of the streaming design: naming the element and the framing is everything a
      * description can honestly say, and what a sequence of them is belongs to the interpreter that has an effect type
      * to say it in. An endpoint carrying one is handed its stream by the backend alongside the value it decoded, and
      * `Streamed` keeping the element in its own type is what lets that backend pin the element type in the compiler
      * rather than in a comment.
      */
    final case class Streamed[+S[-w, +r], -W, +R](
        override val mediaType: MediaType,
        frame: Frame,
        element: Reference[S, W, R]
    ) extends Body.Value[S, Unit, Unit]

    final case class Modify[+S[-w, +r], W0, R0, -W, +R](
        self: Body.Value[S, W0, R0],
        f: R0 => R,
        g: W => W0
    ) extends Body.Value[S, W, R]:
      export self.mediaType

    given profunctor: [S[-w, +r]] => Profunctor[[w, r] =>> Body.Value[S, w, r]]:
      override def dimap[W0, R0, W, R](self: Body.Value[S, W0, R0])(f: W => W0)(g: R0 => R): Body.Value[S, W, R] =
        Body.Value.Modify(self, g, f)
