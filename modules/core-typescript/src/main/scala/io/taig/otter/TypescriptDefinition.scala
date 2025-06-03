package io.taig.otter

import cats.Show
import cats.syntax.all.*

final case class TypescriptDefinition[A <: Typescript](name: String, value: A, exported: Boolean):
  override def toString: String =
    if exported then show"""export type $name = $value"""
    else show"""type $name = $value"""

object TypescriptDefinition:
  def apply[A <: Typescript](name: String, value: A): TypescriptDefinition[A] =
    TypescriptDefinition(name, value, exported = true)

  given Show[TypescriptDefinition[?]] = Show.fromToString
