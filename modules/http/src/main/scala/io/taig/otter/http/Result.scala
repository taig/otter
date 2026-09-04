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

/** One of the answers an endpoint may give, round tripping `A`. */
type Result[A] = Result.Of[Body.Payload, A]

object Result:
  /** A result holding the payload `S` and round tripping `A`.
    *
    * The same shape as a [[Request]] with the method and the path taken away and a status [[Code]] put in their place,
    * for the same reason: an answer is a code, some headers and at most one entity.
    */
  type Of[S[-w, +r], A] = Result.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Result.Schema[Body.Payload, w, r]

  type Reader[+A] = Result.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Result.Schema[S, Nothing, A]

  type Writer[-A] = Result.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Result.Schema[S, A, Any]

  final case class Schema[+S[-w, +r], -W, +R](self: Annotation[Result.Value[S, W, R]]):
    export self.self.{bodies, code, headers, streamed}

  object Schema:
    def apply[S[-w, +r], W, R](self: Result.Value[S, W, R]): Result.Schema[S, W, R] =
      new Result.Schema(Annotation(self))

    given annotated: [S[-w, +r], W, R] => Annotated[Result.Schema[S, W, R]]:
      extension (self: Result.Schema[S, W, R])
        override def lens: (Metadata, Metadata => Result.Schema[S, W, R]) =
          (self.self.metadata, metadata => new Result.Schema(self.self.copy(metadata = metadata)))

    given profunctor: [S[-w, +r]] => Profunctor[[w, r] =>> Result.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Result.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Result.Schema[S, W, R] =
        new Result.Schema(self.self.map(Result.Value.Modify(_, g, f)))

    given functor: [S[-w, +r]] => Functor[[a] =>> Result.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Result.Schema[S, w, r]]

    given contravariant: [S[-w, +r]] => Contravariant[[a] =>> Result.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Result.Schema[S, w, r]]

    given invariant: [S[-w, +r]] => Invariant[[a] =>> Result.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Result.Schema[S, w, r]]

    given unionable: [S[-w, +r]]
      => UnionableOperation[[w, r] =>> Result.Schema[S, w, r], [w, r] =>> Results.Schema[S, w, r]] =
      UnionableOperation.derived

    /** `result :+ result`. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          [w, r] =>> Result.Schema[S1, w, r],
          [w, r] =>> Results.Schema[Body.Or[S1, S2], w, r],
          [w, r] =>> Result.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Result.Schema[S1, W, R]): Results.Schema[Body.Or[S1, S2], W, R] =
        Results.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

      override def element[W, R](fb: => Result.Schema[S2, W, R]): Results.Schema[Body.Or[S1, S2], W, R] =
        Results.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  sealed abstract class Value[+S[-w, +r], -W, +R]:
    def code: Code

    def headers: Option[Reference[Headers.Node, ?, ?]]

    def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]]

    def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], ?, ?]]

  object Value:
    final case class Root(override val code: Code) extends Result.Value[Nothing, Unit, Unit]:
      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = None

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[Nothing, w, r], ?, ?]] = None

      override def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[Nothing, w, r], ?, ?]] = None

    final case class Headers[+S[-w, +r], W1, R1, W2, R2](
        self: Result.Value[S, W1, R1],
        values: Reference[io.taig.otter.http.Headers.Node, W2, R2]
    ) extends Result.Value[S, (W1, W2), (R1, R2)]:
      export self.{bodies, code, streamed}

      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = Some(values)

    final case class Payload[+S[-w, +r], W1, R1, W2, R2](
        self: Result.Value[S, W1, R1],
        values: Reference[[w, r] =>> Bodies.Schema[S, w, r], W2, R2]
    ) extends Result.Value[S, (W1, W2), (R1, R2)]:
      export self.{code, headers, streamed}

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]] = Some(values)

    /** A streamed body added to a result, which changes what it describes without changing what it holds. */
    final case class Streaming[+S[-w, +r], W1, R1, W2, R2](
        self: Result.Value[S, W1, R1],
        value: Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], W2, R2]
    ) extends Result.Value[S, W1, R1]:
      export self.{bodies, code, headers}

      override def streamed: Option[Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], ?, ?]] = Some(value)

    final case class Modify[+S[-w, +r], W0, R0, -W, +R](self: Result.Value[S, W0, R0], f: R0 => R, g: W => W0)
        extends Result.Value[S, W, R]:
      export self.{bodies, code, headers, streamed}
