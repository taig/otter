package io.taig.otter

import cats.Show
import cats.syntax.all.*

final case class TypescriptDefinition[A <: Typescript](name: String, value: A):
  override def toString: String = show"""export type $name = $value"""

object TypescriptDefinition:
  given Show[TypescriptDefinition[?]] = Show.fromToString
