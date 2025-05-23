package io.taig.otter

import cats.syntax.all.*
import cats.Show

final case class TypescriptDefinition(name: String, value: Typescript):
  override def toString: String = show"""export type $name = $value"""

object TypescriptDefinition:
  given Show[TypescriptDefinition] = Show.fromToString
