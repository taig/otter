package io.taig.otter

import cats.data.NonEmptyList
import io.taig.otter.codec.JsonTypescriptTarget

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

  val Target: JsonTypescriptTarget = new JsonTypescriptTarget:
    override def inferred(symbol: Typescript.Expression): Typescript.Type =
      TypescriptEffect.inferred(Typescript.Type.TypeOf(symbol))

    override def annotation(name: String): Typescript.Type =
      TypescriptEffect.annotation(Typescript.Type.Symbol(name, parameters = Nil))

    override def suspend(self: Typescript.Expression): Typescript.Expression = TypescriptEffect.suspend(self)
