package io.taig.otter

private def Schema(property: Typescript.Expression): Typescript.Expression =
  Typescript.Expression.Member(namespace = "Schema", property)

private def Schema(property: Typescript.Type): Typescript.Type =
  Typescript.Type.Member(namespace = "Schema", property)
