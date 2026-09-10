package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Side
import io.taig.otter.Typescript

/** The expressions and type projections a target schema library contributes to generation. */
trait JsonTypescriptTarget:
  def inferred(symbol: Typescript.Expression): Typescript.Type

  def encoded(symbol: Typescript.Expression): Typescript.Type

  def annotation(decoded: Typescript.Type, encoded: Typescript.Type): Typescript.Type

  def structural(
      side: Side,
      projection: JsonTypescriptTarget.Projection,
      renderer: Renderer[Json.Node, Typescript.Type]
  ): Renderer[Json.Node, Typescript.Type]

  def suspend(self: Typescript.Expression): Typescript.Expression

object JsonTypescriptTarget:
  enum Projection:
    case Decoded, Encoded
