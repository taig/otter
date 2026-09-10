package io.taig.otter.codec

import io.taig.otter.Typescript

/** A schema hoisted out of where it was used and given a name of its own.
  *
  * `annotation` is what the constant is ascribed to, and it is present exactly when the type could not be inferred from
  * the value -- which is when the value refers to itself, because TypeScript will not follow a cycle back to its start.
  */
final case class JsonTypescriptDefinition(
    tpe: Typescript.Type,
    annotation: Option[Typescript.Type],
    expression: Typescript.Expression,
    encoded: Option[(String, Typescript.Type)] = None
):
  def declarations(name: String): List[Typescript.Statement.Declaration] =
    val types = Typescript.Statement.Declaration.Type(exported = true, name, tpe) :: encoded.toList.map: (name, tpe) =>
      Typescript.Statement.Declaration.Type(exported = true, name, tpe)

    types :+ Typescript.Statement.Declaration.Constant(exported = true, name, annotation, expression)
