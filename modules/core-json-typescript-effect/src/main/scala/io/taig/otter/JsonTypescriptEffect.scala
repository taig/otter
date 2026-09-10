package io.taig.otter

import cats.data.NonEmptyList
import io.taig.otter.codec.JsonTypescriptContext
import io.taig.otter.codec.JsonTypescriptTarget
import io.taig.otter.codec.JsonTypescriptTypeEffectRenderer
import io.taig.otter.codec.Renderer

/** How the `effect` `Schema` module answers what [[io.taig.otter.codec.JsonStateTypescriptRenderer]] asks of a target.
  */
object JsonTypescriptEffect:
  val Namespace: Metadata.Namespace = Metadata.Namespace("json-typescript-effect")

  /** The namespaces an attribute is looked up under, most specific first: what this generator alone should see, then
    * what any effect generator should, then any JSON TypeScript one, then any TypeScript one, then what every format is
    * told.
    */
  val Namespaces: NonEmptyList[Metadata.Namespace] = NonEmptyList.of(
    JsonTypescriptEffect.Namespace,
    TypescriptEffect.Namespace,
    JsonTypescript.Namespace,
    Typescript.Namespace,
    Metadata.Namespace.Global
  )

  val Context: JsonTypescriptContext = JsonTypescriptContext.Empty.copy(reserved = TypescriptEffect.Reserved)

  val Target: JsonTypescriptTarget = new JsonTypescriptTarget:
    override def inferred(symbol: Typescript.Expression): Typescript.Type =
      TypescriptEffect.inferred(Typescript.Type.TypeOf(symbol))

    override def encoded(symbol: Typescript.Expression): Typescript.Type =
      TypescriptEffect.encoded(Typescript.Type.TypeOf(symbol))

    override def annotation(decoded: Typescript.Type, encoded: Typescript.Type): Typescript.Type =
      TypescriptEffect.annotation(decoded, encoded)

    override def structural(
        side: Side,
        projection: JsonTypescriptTarget.Projection,
        renderer: Renderer[Json.Node, Typescript.Type]
    ): Renderer[Json.Node, Typescript.Type] = JsonTypescriptTypeEffectRenderer(side, projection, renderer)

    override def suspend(self: Typescript.Expression): Typescript.Expression = TypescriptEffect.suspend(self)
