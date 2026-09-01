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
    expression: Typescript.Expression
):
  def declarations(name: String): List[Typescript.Statement.Declaration] =
    Typescript.Statement.Declaration.Type(exported = true, name, tpe) ::
      Typescript.Statement.Declaration.Constant(exported = true, name, annotation, expression) ::
      Nil
