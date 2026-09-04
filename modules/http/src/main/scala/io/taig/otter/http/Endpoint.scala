package io.taig.otter.http

import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Metadata

/** An endpoint that takes `A` and answers with `B`. */
type Endpoint[A, B] = Endpoint.Of[Body.Payload, A, B]

object Endpoint:
  /** An endpoint holding the payload `S`, taking `A` and answering with `B`. */
  type Of[S[-w, +r], A, B] = Endpoint.Schema[S, A, A, B, B]

  /** An endpoint as the side that answers it sees it: it reads the request and writes the response.
    *
    * This is [[io.taig.otter.Side]] one tier up, and it is what makes an endpoint describable once and rendered
    * correctly twice. The two sides of a schema genuinely differ -- wherever a field is optional or holds a default,
    * what a reader accepts is not what a writer produces -- so a document written for whoever calls this endpoint has
    * to describe the request as it is *read* and the response as it is *written*. Neither earlier attempt drew this
    * distinction, and a renderer without it has to be told twice which way round it is looking.
    */
  type Server[S[-w, +r], A, B] = Endpoint.Schema[S, Nothing, A, B, Any]

  /** An endpoint as the side that calls it sees it: it writes the request and reads the response. */
  type Client[S[-w, +r], A, B] = Endpoint.Schema[S, A, Any, Nothing, B]

  /** Whatever it holds, which is the form a renderer is written against.
    *
    * All four sides are free because a renderer touches no value: it is handed a schema and asked what document
    * describes it, so every endpoint widens to this.
    */
  type Node = Endpoint.Schema[Body.Payload, Nothing, Any, Nothing, Any]

  final case class Schema[+S[-w, +r], -AW, +AR, -BW, +BR](self: Annotation[Endpoint.Value[S, AW, AR, BW, BR]]):
    export self.self.{request, responses}

  object Schema:
    def apply[S[-w, +r], AW, AR, BW, BR](
        self: Endpoint.Value[S, AW, AR, BW, BR]
    ): Endpoint.Schema[S, AW, AR, BW, BR] = new Endpoint.Schema(Annotation(self))

    given annotated: [S[-w, +r], AW, AR, BW, BR] => Annotated[Endpoint.Schema[S, AW, AR, BW, BR]]:
      extension (self: Endpoint.Schema[S, AW, AR, BW, BR])
        override def lens: (Metadata, Metadata => Endpoint.Schema[S, AW, AR, BW, BR]) =
          (self.self.metadata, metadata => new Endpoint.Schema(self.self.copy(metadata = metadata)))

  final case class Value[+S[-w, +r], -AW, +AR, -BW, +BR](
      request: Request.Schema[S, AW, AR],
      responses: Results.Schema[S, BW, BR]
  )
