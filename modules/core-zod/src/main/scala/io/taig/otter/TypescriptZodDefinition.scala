// package io.taig.otter

// import cats.Show
// import cats.syntax.all.*

// final case class TypescriptZodDefinition(typescript: TypescriptDefinition[?], expression: String):
//   override def toString: String =
//     show"""$typescript
//           |export const ${typescript.name}: z.ZodType<${typescript.name}> =
//           |${indent(expression)}""".stripMargin

// object TypescriptZodDefinition:
//   given Show[TypescriptZodDefinition] = Show.fromToString
