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

/** A body with requirement `S` that round trips `A`. */
type Body[S[-w, +r], A] = Body.Of[S, A]

object Body:
  /** Any body requirement, used by document renderers and other inspection APIs. */
  type Payload = [w, r] =>> Any

  /** A whole document requires an interpreter for its payload alphabet. */
  type Whole[P[-w, +r]] = [w, r] =>> Body.Requirement.Whole[P, w, r]

  object Requirement:
    sealed abstract class Whole[+P[-_, +_], -W, +R]

    sealed abstract class Streamed[C[+_], +P[-_, +_], -W, +R]

  /** The payload of a body that has none, which is not the same as a body that is not there.
    *
    * Uninhabited, so it says what `Nothing` said before it -- there is no document here, as against an empty one. It is
    * a type of its own because `Nothing` is also what a request with no entity yet holds, and one type meaning both is
    * what let `request(...)(body.binary(...))(...)` typecheck: a binary body left the request looking body free, and
    * the second entity then shadowed the first in [[Body.Value]] while both stayed in what the request holds.
    *
    * A declared constructor rather than an alias to `Unit`, for the reason the note above gives. `S` is a type
    * constructor, so `Unit` is the wrong kind; `[w, r] =>> Unit` is the right kind and still does not conform to
    * [[Body.Payload]] where alternatives are unioned, an alias's own parameters being invariant where `w` is not. Only
    * a class or a trait carries the variance the position asks for.
    */
  sealed abstract class Opaque[-W, +R]

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
  final case class Schema[+S[-_, +_], -W, +R](self: Annotation[Body.Value[S, W, R]]):
    def mediaType: MediaType = self.self.mediaType

  object Schema:
    def apply[S[-_, +_], W, R](self: Body.Value[S, W, R]): Body.Schema[S, W, R] =
      new Body.Schema(Annotation(self))

    given annotated: [S[-w, +r], W, R] => Annotated[Body.Schema[S, W, R]]:
      extension (self: Body.Schema[S, W, R])
        override def lens: (Metadata, Metadata => Body.Schema[S, W, R]) =
          (self.self.metadata, metadata => new Body.Schema(self.self.copy(metadata = metadata)))

    given profunctor: [S[-w, +r]] => Profunctor[Body.Schema[S, *, *]]:
      override def dimap[W0, R0, W, R](
          self: Body.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Body.Schema[S, W, R] =
        new Body.Schema(self.self.map(Body.Value.profunctor.dimap(_)(f)(g)))

    given functor: [S[-w, +r]] => Functor[Body.Schema[S, Nothing, *]] =
      Direction.functor[Body.Schema[S, *, *]]

    given contravariant: [S[-w, +r]] => Contravariant[Body.Schema[S, *, Any]] =
      Direction.contravariant[Body.Schema[S, *, *]]

    given invariant: [S[-w, +r]] => Invariant[[a] =>> Body.Schema[S, a, a]] =
      Direction.invariant[Body.Schema[S, *, *]]

    given unionable: [S[-w, +r]]
      => UnionableOperation[Body.Schema[S, *, *], Bodies.Schema[S, *, *]] =
      UnionableOperation.derived

    /** `body :+ body`. The result carries both children's payload, so the union accumulates down the chain. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          Body.Schema[S1, *, *],
          Bodies.Schema[Body.Or[S1, S2], *, *],
          Body.Schema[S2, *, *]
        ]:
      override def lift[W, R](fa: Body.Schema[S1, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] =
        Bodies.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

      override def element[W, R](fb: => Body.Schema[S2, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] =
        Bodies.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  /** A stream carrier is chosen by the backend, without introducing an effect into this alphabet. */
  object Streamed:
    type Requirement[C[+_], P[-w, +r]] = [w, r] =>> Body.Requirement.Streamed[C, P, w, r]

  /** Capability required by a raw byte stream. */
  sealed abstract class Raw[-W, +R]

  /** What a body is, and the profunctor that maps its values.
    *
    * A [[Body.Value.Whole]] is one document. A [[Body.Value.Binary]] is bytes with no document in them at all -- an
    * image, a PDF -- carried as a `ByteVector` so that comparing two of them means comparing their contents. A
    * [[Body.Value.Streamed]] is a sequence of documents arriving one at a time. [[Body.Value.Raw]] carries typed bytes
    * directly, without a document codec or framing.
    */
  sealed abstract class Value[+S[-_, +_], -W, +R]:
    def mediaType: MediaType

  object Value:
    /** One document, read and written whole. */
    final case class Whole[+S[-_, +_], -W, +R](
        override val mediaType: MediaType,
        payload: Reference[S, W, R]
    ) extends Body.Value[Body.Whole[S], W, R]

    /** Bytes, with no schema to describe them.
      *
      * `ByteVector` rather than `Array[Byte]`, which has reference equality and would make a body holding a literal
      * impossible to compare and a golden test impossible to write.
      */
    final case class Binary(override val mediaType: MediaType) extends Body.Value[Body.Opaque, ByteVector, ByteVector]

    /** Framed documents carried by an abstract covariant stream. */
    final case class Streamed[C[+_], +S[-_, +_], -W, +R](
        override val mediaType: MediaType,
        frame: Frame,
        element: Reference[S, W, R]
    ) extends Body.Value[Body.Streamed.Requirement[C, S], C[W], C[R]]:
      require(frame != Frame.Raw, "Use body.streamed[C].raw for raw byte streams")
      frame match
        case Frame.Delimited(separator) => require(separator.nonEmpty, "A stream delimiter must not be empty")
        case _                          => ()

    final case class Raw[C[+_]](override val mediaType: MediaType)
        extends Body.Value[Body.Streamed.Requirement[C, Body.Raw], C[Byte], C[Byte]]

    final case class Modify[+S[-_, +_], W0, R0, -W, +R](
        self: Body.Value[S, W0, R0],
        f: R0 => R,
        g: W => W0
    ) extends Body.Value[S, W, R]:
      export self.mediaType

    given profunctor: [S[-w, +r]] => Profunctor[Body.Value[S, *, *]]:
      override def dimap[W0, R0, W, R](self: Body.Value[S, W0, R0])(f: W => W0)(g: R0 => R): Body.Value[S, W, R] =
        Body.Value.Modify(self, g, f)
