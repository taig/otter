package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.ZodExpression
import io.taig.otter.http.Endpoint

import scala.collection.immutable.ListMap

final class ZodEndpointsRenderer(imports: List[String]):
  def render(endpoints: List[Endpoint[Json, Json, Json, ?, ?]]): String =
    val (references, definitions) = endpoints
      .traverse(ZodEndpointRenderer.render)
      .run(initial = ListMap.empty)
      .value
      .leftMap(_.toList.map[ZodExpression.Referenced](ZodExpression.Referenced.apply).map(ZodExpressionRenderer.render))

    s"""/* Imports */
       |import { z } from "zod"
       |${imports.mkString("\n")}
       |
       |/* Definitions */
       |export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (code: number, body: () => Promise<any>) => Promise<A>
       |}
       |
       |/* Types */
       |${references.mkString("\n\n")}
       |
       |/* Endpoints */
       |${definitions.mkString("\n\n")}""".stripMargin
