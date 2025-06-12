package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.http.Endpoint
import io.taig.otter.ContextState
import io.taig.otter.TypescriptEffectDefinition

final class EndpointsTypescriptEffectRenderer(imports: List[String]):
  def render(endpoints: List[Endpoint[Json, ?, ?]]): String =
    val (context, result) = endpoints
      .traverse(EndpointTypescriptEffectRenderer.render)
      .run(initial = ContextState.Context.Empty)
      .value

    s"""/* Imports */
       |
       |import { z } from "Effect"
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
       |${context.references.toList.map(TypescriptEffectDefinition.apply).mkString_("\n\n")}
       |
       |/* Endpoints */
       |
       |""".stripMargin
