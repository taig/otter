package io.taig.otter.codec

import io.taig.otter.http.Endpoint
import io.taig.otter.Json
import cats.syntax.all.*
import scala.collection.immutable.ListMap
import io.taig.otter.ZodExpression

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
       |  handle: (response: Response) => Promise<A>
       |}
       |
       |/* Types */
       |${references.mkString("\n\n")}
       |
       |/* Endpoints */
       |${definitions.mkString("\n\n")}""".stripMargin
