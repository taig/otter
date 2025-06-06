package io.taig.otter

import cats.Show

final case class TypescriptZodDefinition(name: String, value: TypescriptZod):
  override def toString: String = value match
    case TypescriptZod.Shared(self)           => ???
    case TypescriptZod.Split(typescript, zod) => ???

object TypescriptZodDefinition:
  given Show[TypescriptZodDefinition] = Show.fromToString
