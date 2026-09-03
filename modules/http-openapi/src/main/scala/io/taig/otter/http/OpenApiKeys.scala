package io.taig.otter.http

import io.taig.otter.Metadata

/** The attributes an OpenAPI document reads that no other renderer has a use for.
  *
  * [[io.taig.otter.Keys.title]] and [[io.taig.otter.Keys.description]] are already read, by the JSON Schema renderer
  * this one delegates its payloads to, so they are not restated here.
  */
trait OpenApiKeys:
  /** The name a tool generating a client calls this operation. Falls back to `METHOD /path` when a schema says nothing.
    */
  val operationId: Metadata.Key[String] = Metadata.Key("operationId")

  /** A one line label for an operation, which OpenAPI keeps apart from its description. */
  val summary: Metadata.Key[String] = Metadata.Key("summary")

  /** The groups an operation is listed under. */
  val tags: Metadata.Key[List[String]] = Metadata.Key("tags")

object OpenApiKeys extends OpenApiKeys
