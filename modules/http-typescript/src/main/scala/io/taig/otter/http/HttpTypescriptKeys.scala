package io.taig.otter.http

import io.taig.otter.Metadata

/** The attributes a generated endpoint reads.
  *
  * A trait and an object, the way [[io.taig.otter.Keys]] is, so that a downstream module adds its own vocabulary beside
  * this one rather than editing an enumeration.
  */
trait HttpTypescriptKeys:
  /** The preferred name of the generated descriptor. Invalid identifier characters are replaced and collisions are
    * suffixed. Falls back to a name derived from the method and the path, which is the fallback
    * [[OpenApiKeys.operationId]] already describes -- and is deliberately the same key by name, so that an endpoint
    * that has told an OpenAPI document what to call it has told this renderer too.
    */
  val operationId: Metadata.Key[String] = Metadata.Key("operationId")

object HttpTypescriptKeys extends HttpTypescriptKeys
