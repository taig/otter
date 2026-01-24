package io.taig.otter

private object z:
  def apply(typescript: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "z", typescript)

  def apply(typescript: Typescript.Type): Typescript.Type =
    Typescript.Type.Member(namespace = "z", typescript)

  def infer(typescript: Typescript.Type): Typescript.Type =
    z(Typescript.Type.Symbol("infer", List(typescript)))

  def unapply(typescript: Typescript.Type): Option[Typescript.Type] =
    PartialFunction.condOpt(typescript):
      case Typescript.Type.Member("z", property) => property
