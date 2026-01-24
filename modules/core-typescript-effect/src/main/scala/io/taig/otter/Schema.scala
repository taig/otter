package io.taig.otter

private object Schema:
  def apply(expression: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "Schema", expression)

  def apply(tpe: Typescript.Type): Typescript.Type =
    Typescript.Type.Member(namespace = "Schema", tpe)

  def suspend(expression: Typescript.Expression): Typescript.Expression =
    Schema(
      Typescript.Expression.Call(
        name = "suspend",
        arguments = List(Typescript.Expression.Arrow(body = expression))
      )
    )

  def tpe(tpe: Typescript.Type): Typescript.Type =
    Schema(Schema(Typescript.Type.Symbol("Type", List(tpe))))

  def unapply(tpe: Typescript.Type): Option[Typescript.Type] =
    PartialFunction.condOpt(tpe):
      case Typescript.Type.Member("Schema", property) => property
