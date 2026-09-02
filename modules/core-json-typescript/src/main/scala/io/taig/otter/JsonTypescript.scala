package io.taig.otter

/** What every TypeScript renderer of a JSON schema needs before it can pick a target library. */
object JsonTypescript:
  /** The [[Metadata.Namespace]] the JSON TypeScript renderers read their attributes from, whatever the target library.
    */
  val Namespace: Metadata.Namespace = Metadata.Namespace("json-typescript")
