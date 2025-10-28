package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.JsonSchemaExpression

import scala.collection.immutable.SortedMap

object JsonSchemaRenderer:
  def apply(encoder: Encoder[Json, CirceJson]): Renderer[Json, CirceJson] =
    JsonSchemaExpressionRenderer(encoder).map: state =>
      val (definitions, expression) = state.run(SortedMap.empty).value
      expression.asJson.deepMerge(CirceJson.obj("$defs" := definitions).dropEmptyValues)
