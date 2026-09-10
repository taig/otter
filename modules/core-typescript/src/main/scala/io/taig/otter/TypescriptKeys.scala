package io.taig.otter

/** Attributes a schema can carry to say what it renders as, overriding what a renderer would have derived.
  *
  * The escape hatch for everything the derivation cannot know: a text format that has a refined counterpart in the
  * target library, a primitive that should arrive as something narrower, a type the generator has no way to name.
  */
trait TypescriptKeys:
  val expression: Metadata.Key[Typescript.Expression] = Metadata.Key("expression")

  /** The decoded type of a generated schema. */
  val tpe: Metadata.Key[Typescript.Type] = Metadata.Key("type")

  /** The encoded type; when absent, the generator derives the wire representation. */
  val encodedType: Metadata.Key[Typescript.Type] = Metadata.Key("encoded-type")

object TypescriptKeys extends TypescriptKeys
