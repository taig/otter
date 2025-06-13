package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.ContextState
import io.taig.otter.Json
import io.taig.otter.TypescriptEffectDefinition
import io.taig.otter.http.Endpoint

final class EndpointsTypescriptEffectRenderer(imports: List[String]):
  def render(endpoints: List[Endpoint[Json, ?, ?]]): String =
    val (context, results) = endpoints
      .traverse(EndpointTypescriptEffectRenderer.render)
      .run(initial = ContextState.Context.Empty)
      .value

    s"""/* Imports */
       |
       |import { Schema } from "Effect"
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
       |${results.mkString_("\n\n")}""".stripMargin
