package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.TypescriptState
import io.taig.otter.TypescriptZodDefinition
import io.taig.otter.TypescriptZodState
import io.taig.otter.http.Endpoint

final class TypescriptZodEndpointsRenderer(imports: List[String]):
  def render(endpoints: List[Endpoint[Json, ?, ?]]): String =
    val (references, result) = endpoints
      .traverse(TypescriptZodEndpointRenderer.render)
      .run(initial = TypescriptState.Context.Empty)
      .value
      .leftMap(_.references)

    val (context, zod) = result
      .traverse: endpoint =>
        endpoint.traverse: typescript =>
          TypescriptZodEncoder
            .encode(references, typescript = typescript.value)
            .map(TypescriptZodDefinition(typescript, _))
      .run(TypescriptZodState.Context.Empty)
      .value

    s"""/* Imports */
       |
       |import { z } from "zod"
       |${imports.mkString("\n")}
       |
       |/* Definitions */
       |
       |export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (code: number, headers: Headers, body: () => Promise<any>) => Promise<A>
       |}
       |
       |/* Types */
       |
       |${context.definitions.mkString_("\n\n")}
       |
       |/* Endpoints */
       |
       |${zod.mkString_("\n\n")}""".stripMargin
