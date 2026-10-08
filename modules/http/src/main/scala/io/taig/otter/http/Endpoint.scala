package io.taig.otter.http

import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Metadata

/** An endpoint with requirement `S` that takes `A` and answers with `B`. */
type Endpoint[S[-w, +r], A, B] = Endpoint.Of[S, A, B]

object Endpoint:
  /** An endpoint holding the payload `S`, taking `A` and answering with `B`. */
  type Of[S[-w, +r], A, B] = Endpoint.Schema[S, S, A, A, B, B]

  /** An endpoint as the side that answers it sees it: it reads the request and writes the response.
    *
    * This is [[io.taig.otter.Side]] one tier up, and it is what makes an endpoint describable once and rendered
    * correctly twice. The two sides of a schema genuinely differ -- wherever a field is optional or holds a default,
    * what a reader accepts is not what a writer produces -- so a document written for whoever calls this endpoint has
    * to describe the request as it is *read* and the response as it is *written*. Neither earlier attempt drew this
    * distinction, and a renderer without it has to be told twice which way round it is looking.
    */
  type Server[S[-w, +r], A, B] = Endpoint.Schema[S, S, Nothing, A, B, Any]

  /** An endpoint as the side that calls it sees it: it writes the request and reads the response. */
  type Client[S[-w, +r], A, B] = Endpoint.Schema[S, S, A, Any, Nothing, B]

  /** Whatever it holds, which is the form a renderer is written against.
    *
    * All four sides are free because a renderer touches no value: it is handed a schema and asked what document
    * describes it, so every endpoint widens to this.
    */
  type Node = Endpoint.Schema[Body.Payload, Body.Payload, Nothing, Any, Nothing, Any]

  /** A domain endpoint with optional error declarations, composed only by its consumer. */
  sealed trait Declaration[+Q[-_, +_], +S[-_, +_], -AW, +AR, -BW, +BR, +E]:
    def domain: Endpoint.Schema[Q, S, AW, AR, BW, BR]
    def overrides: ErrorOverrides[S, E]

    final def compose[T[-w, +r] >: S[w, r], F](
        defaults: ErrorPolicy[T, F]
    ): ComposedEndpoint[Q, T, AW, AR, BW, BR, E | F] =
      ComposedEndpoint(domain, overrides(defaults))

  object Declaration:
    type Node = Endpoint.Declaration[Body.Payload, Body.Payload, Nothing, Any, Nothing, Any, Any]

  final case class WithErrors[+Q[-_, +_], +S[-_, +_], -AW, +AR, -BW, +BR, +E](
      override val domain: Endpoint.Schema[Q, S, AW, AR, BW, BR],
      override val overrides: ErrorOverrides[S, E]
  ) extends Endpoint.Declaration[Q, S, AW, AR, BW, BR, E]

  object WithErrors:
    given annotated
        : [Q[-w, +r], S[-w, +r], AW, AR, BW, BR, E] => Annotated[Endpoint.WithErrors[Q, S, AW, AR, BW, BR, E]]:
      extension (self: Endpoint.WithErrors[Q, S, AW, AR, BW, BR, E])
        override def lens: (Metadata, Metadata => Endpoint.WithErrors[Q, S, AW, AR, BW, BR, E]) =
          (
            self.domain.self.metadata,
            metadata => self.copy(domain = new Endpoint.Schema(self.domain.self.copy(metadata = metadata)))
          )

  final case class Schema[+Q[-_, +_], +S[-_, +_], -AW, +AR, -BW, +BR](
      self: Annotation[Endpoint.Value[Q, S, AW, AR, BW, BR]]
  ) extends Endpoint.Declaration[Q, S, AW, AR, BW, BR, Nothing]:
    override def domain: Endpoint.Schema[Q, S, AW, AR, BW, BR] = this
    override def overrides: ErrorOverrides[Nothing, Nothing] = ErrorOverrides(
      envelope = None,
      syntax = None,
      contentType = None,
      validation = None,
      entityRead = None,
      encoding = None,
      status = None,
      unexpected = None
    )

    export self.self.{request, responses}

  object Schema:
    def apply[Q[-_, +_], S[-_, +_], AW, AR, BW, BR](
        self: Endpoint.Value[Q, S, AW, AR, BW, BR]
    ): Endpoint.Schema[Q, S, AW, AR, BW, BR] = new Endpoint.Schema(Annotation(self))

    given annotated: [Q[-w, +r], S[-w, +r], AW, AR, BW, BR] => Annotated[Endpoint.Schema[Q, S, AW, AR, BW, BR]]:
      extension (self: Endpoint.Schema[Q, S, AW, AR, BW, BR])
        override def lens: (Metadata, Metadata => Endpoint.Schema[Q, S, AW, AR, BW, BR]) =
          (self.self.metadata, metadata => new Endpoint.Schema(self.self.copy(metadata = metadata)))

  final case class Value[+Q[-_, +_], +S[-_, +_], -AW, +AR, -BW, +BR](
      request: Request.Schema[Q, AW, AR],
      responses: Responses.Schema[S, BW, BR]
  )
