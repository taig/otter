package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.http.Endpoint

object DataEndpointPrinter:
  def apply(endpoint: Endpoint[?, ?]): String =
    endpoint.request.bodies.map(_.toNev).map(_.map(body => body.codec))
    ZodCodecPrinter()
    s"""export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (response: Response) => Promise<A>
       |}
       |
       |const ${endpoint.request.method} = ({}): Request<any> => ({
       |  method: ${endpoint.request.method},
       |  path: "/"
       |})""".stripMargin
