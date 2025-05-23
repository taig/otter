package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.http.Endpoint
import io.taig.otter.ZodState
import io.taig.otter.TypescriptState
import io.taig.otter.TypescriptZodDefinition

final class TypescriptZodEndpointsRenderer(imports: List[String]):
  def render(endpoints: List[Endpoint[Json, Json, Json, ?, ?]]): String =
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
      .run(ZodState.Context.Empty)
      .value

    // val (typesContext, types) = result
    //   .traverse: endpoint =>
    //     TypescriptZodEncoder
    //       .encode(references = context.references, typescript = endpoint.input.value)
    //       .map(TypescriptZodDefinition(typescript = endpoint.input, _))
    //   .run(ZodState.Context.Empty)
    //   .value

    s"""/* Imports */
       |
       |import { z } from "zod"
       |${imports.mkString("\n")}
       |
       |/* Definitions */
       |
       |export type Request<A, B, C> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (code: number, body: () => Promise<any>) => Promise<Response<A, B, C>>
       |}
       |
       |export type Response<A, B, C> =
       |  { type: "result", value: A } |
       |  { type: "violation", value: B } |
       |  { type: "failure", value: C }
       |
       |/* Types */
       |
       |${context.definitions.mkString_("\n\n")}
       |
       |/* Endpoints */
       |
       |${zod.mkString_("\n\n")}""".stripMargin
