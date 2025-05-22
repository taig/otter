// package io.taig.otter.codec

// import cats.syntax.all.*
// import io.taig.otter.Json
// import io.taig.otter.ZodExpression
// import io.taig.otter.http.Endpoint
// import io.taig.otter.ZodState

// final class ZodEndpointsRenderer(imports: List[String]):
//   def render(endpoints: List[Endpoint[Json, Json, Json, ?, ?]]): String =
//     val (references, definitions) = endpoints
//       .traverse(ZodEndpointRenderer.render)
//       .run(initial = ZodState.Context.Empty)
//       .value
//       .leftMap(
//         _.references.toList
//           .map[ZodExpression.Referenced](ZodExpression.Referenced.apply)
//           .map(ZodExpressionRenderer.render)
//       )

//     s"""/* Imports */
//        |
//        |import { z } from "zod"
//        |${imports.mkString("\n")}
//        |
//        |/* Definitions */
//        |
//        |export type Request<A, B, C> = {
//        |  method: string
//        |  path: string
//        |  headers?: HeadersInit
//        |  body?: BodyInit | null
//        |  handle: (code: number, body: () => Promise<any>) => Promise<Response<A, B, C>>
//        |}
//        |
//        |export type Response<A, B, C> =
//        |  { type: "result", value: A } |
//        |  { type: "violation", value: B } |
//        |  { type: "failure", value: C }
//        |
//        |/* Types */
//        |
//        |${references.mkString("\n\n")}
//        |
//        |/* Endpoints */
//        |
//        |${definitions.mkString("\n\n")}""".stripMargin
