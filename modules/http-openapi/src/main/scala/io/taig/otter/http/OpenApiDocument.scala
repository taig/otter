package io.taig.otter.http

import cats.data.NonEmptyList
import io.circe.Json as CirceJson

/** A rendered OpenAPI document, and every way it falls short of the endpoints it came from.
  *
  * The same shape [[io.taig.otter.JsonSchemaDocument]] has, and for the same reason: refusing to produce a document
  * would make the renderer useless for the whole of an API that is fine because of the one operation that is not.
  */
final case class OpenApiDocument(value: CirceJson, issues: List[OpenApiIssue]):
  /** The document, if the renderer could say everything the endpoints do. */
  def toEither: Either[NonEmptyList[OpenApiIssue], CirceJson] = NonEmptyList.fromList(issues).toLeft(value)
