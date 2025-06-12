// package io.taig.otter

// import cats.Show
// import cats.syntax.all.*
// import io.taig.otter.codec.TypescriptPrinter

// final case class TypescriptDefinition(name: String, value: Typescript):
//   override def toString: String = show"""export type $name = ${TypescriptPrinter.print(value)}"""

// object TypescriptDefinition:
//   given Show[TypescriptDefinition] = Show.fromToString
