package io.taig.otter

private def z(property: Typescript.Expression): Typescript.Expression =
  Typescript.Expression.Member(namespace = "z", property)
