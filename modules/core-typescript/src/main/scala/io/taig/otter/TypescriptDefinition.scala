// package io.taig.otter

// import cats.Show
// import cats.syntax.all.*

// final case class TypescriptDefinition(name: String, value: Typescript):
//   override def toString: String = show"""export type $name = $value"""

// object TypescriptDefinition:
//   given Show[TypescriptDefinition] = Show.fromToString
