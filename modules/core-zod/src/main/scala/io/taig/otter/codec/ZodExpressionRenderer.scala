package io.taig.otter.codec

import io.taig.otter.ZodExpression

object ZodExpressionRenderer:
  def render(expression: ZodExpression.Referenced): String =
    s"""export type ${expression.reference.name} = z.infer<typeof ${expression.reference.name}>
       |export const ${expression.reference.name} = ${expression.value}""".stripMargin
