package io.taig.otter

private object Schema:
  def apply(property: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "Schema", property)

  def apply(property: Typescript.Type): Typescript.Type =
    Typescript.Type.Member(namespace = "Schema", property)

  def unapply(typescript: Typescript.Type): Option[Typescript.Type] =
    PartialFunction.condOpt(typescript):
      case Typescript.Type.Member("Schema", property) => property
