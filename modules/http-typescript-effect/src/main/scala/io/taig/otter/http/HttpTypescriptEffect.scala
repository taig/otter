package io.taig.otter.http

import cats.data.NonEmptyList
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.Metadata

/** How the `effect` `Schema` module answers what a generated endpoint asks of a target. */
object HttpTypescriptEffect:
  /** [[HttpTypescript.Layers]] above the chain the effect payload generator already reads.
    *
    * Spliced rather than restated, so there is one place that says what an endpoint's layers are and one that says what
    * effect's are, and no third place for them to disagree.
    */
  val Namespaces: NonEmptyList[Metadata.Namespace] =
    HttpTypescript.Layers.concatNel(JsonTypescriptEffect.Namespaces)

  /** The module a generated descriptor imports `Schema` from. */
  val Module: String = "effect"
