package io.taig.otter

import cats.data.NonEmptyList
import io.circe.Json as CirceJson

/** A rendered document, and every way it falls short of the schema it came from.
  *
  * The renderer always produces a document. Refusing to produce one would make it useless for the whole of a schema
  * that is fine because of the one corner that is not, and there is no honest substitute to emit in that corner's
  * place. So both come back, and the caller decides whether to care: [[value]] for a consumer that will take what it
  * can get, [[toEither]] for one that will not.
  */
final case class JsonSchemaDocument(value: CirceJson, issues: List[JsonSchemaIssue]):
  /** The document, if the profile could say everything the schema does. */
  def toEither: Either[NonEmptyList[JsonSchemaIssue], CirceJson] =
    NonEmptyList.fromList(issues).toLeft(value)
