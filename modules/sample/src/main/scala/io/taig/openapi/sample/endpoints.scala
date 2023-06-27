package io.taig.openapi.sample

import cats.effect.IO
import io.taig.openapi.http.*
import io.taig.openapi.http4s.Http4s

object endpoints {
  val reqBody: Input.Body.Singlepart[Stream[Byte]] = ???
  val resBody: Output.Body[Int] = ???
  val get: Endpoint[Stream[Byte], Int] = Endpoint(
    Input(Method("GET"), Url.Root, Headers.Empty, reqBody),
    Output(Results(Result(Code(200), Headers.Empty, resBody)), ???)
  )

  final class EndpointImplementation(http4s: Http4s[IO]):
//    def apply[I, O](endpoint: Endpoint[I, O])(f: I => IO[O]): Endpoint.Implementation[IO, I, O] =
//      Endpoint.Implementation(endpoint, f)

    extension [I, O](endpoint: Endpoint[I, O])
      def implementedBy(f: I => IO[O]): Endpoint.Implementation[IO, I, O] =
        ???

    extension [A](stream: Stream[A]) def toFs2: fs2.Stream[IO, A] = ???

  val implementation: EndpointImplementation = ???
  import implementation.*

  get.implementedBy { input =>
    input.toFs2
    IO(3)
  }
}
