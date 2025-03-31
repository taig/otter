// package io.taig.otter

// object ZodPrinter:
//   def print(expression: Expression.Referenced): String =
//     s"""export type ${expression.reference.name} = z.infer<typeof ${expression.reference.name}>
//        |export const ${expression.reference.name} = ${expression.value}""".stripMargin
