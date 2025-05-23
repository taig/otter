package io.taig.otter

final case class TypescriptZod(typescript: Typescript, expression: String):
  def modifyExpression(f: String => String): TypescriptZod = copy(expression = f(expression))

  final def definition(name: String): TypescriptZodDefinition =
    TypescriptZodDefinition(typescript = typescript.definition(name), expression)
