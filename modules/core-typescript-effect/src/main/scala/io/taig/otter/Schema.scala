package io.taig.otter

private object Schema:
  def apply(expression: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "Schema", expression)

  def apply(tpe: Typescript.Type): Typescript.Type =
    Typescript.Type.Member(namespace = "Schema", tpe)

  def suspend(expression: Typescript.Expression): Typescript.Expression =
    Schema(
      Typescript.Expression.Call(
        name = "suspend",
        arguments = List(Typescript.Expression.Arrow(arguments = Nil, body = expression))
      )
    )

  def tpe(tpe: Typescript.Type): Typescript.Type =
    Schema(Schema(Typescript.Type.Symbol("Type", List(tpe))))

  def unapply(tpe: Typescript.Type): Option[Typescript.Type] =
    PartialFunction.condOpt(tpe):
      case Typescript.Type.Member("Schema", property) => property

  val BooleanFromString: Typescript.Expression = Schema(
    Typescript.Expression.Call(
      name = "transform",
      arguments = List(
        Schema(
          Typescript.Expression.Call(
            name = "Union",
            arguments = List(
              Schema(
                Typescript.Expression.Call(
                  name = "Literal",
                  arguments = List(Typescript.Expression.Literal.String("true"))
                )
              ),
              Schema(
                Typescript.Expression.Call(
                  name = "Literal",
                  arguments = List(Typescript.Expression.Literal.String("false"))
                )
              )
            )
          )
        ),
        Schema(Typescript.Expression.Symbol(name = "Boolean")),
        Typescript.Expression.Object(
          fields = List(
            "decode" -> Typescript.Expression.Arrow(
              arguments = List(Typescript.Expression.Symbol("value")),
              body = Typescript.Expression.TripleEqual(
                left = Typescript.Expression.Symbol("value"),
                right = Typescript.Expression.Literal.String("true")
              )
            ),
            "encode" -> Typescript.Expression.Arrow(
              arguments = List(Typescript.Expression.Symbol("value")),
              body = Typescript.Expression.Ternary(
                condition = Typescript.Expression.Symbol("value"),
                valid = Typescript.Expression.Literal.String("true"),
                invalid = Typescript.Expression.Literal.String("false")
              )
            )
          )
        )
      )
    )
  )

  val CoerceBoolean: Typescript.Expression = Schema(
    Typescript.Expression.Call(
      name = "Union",
      arguments = List(
        Schema(Typescript.Expression.Symbol(name = "Boolean")),
        BooleanFromString
      )
    )
  )

  val CoerceNumber: Typescript.Expression = Schema(
    Typescript.Expression.Call(
      name = "Union",
      arguments = List(
        Schema(Typescript.Expression.Symbol(name = "Number")),
        Schema(Typescript.Expression.Symbol(name = "NumberFromString"))
      )
    )
  )

  val CoerceString: Typescript.Expression = Schema(
    Typescript.Expression.Call(
      name = "Union",
      arguments = List(
        Schema(Typescript.Expression.Symbol(name = "String")),
        Schema(
          Typescript.Expression.Call(
            name = "transform",
            arguments = List(
              Schema(Typescript.Expression.Symbol(name = "Number")),
              Schema(Typescript.Expression.Symbol(name = "String")),
              Typescript.Expression.Object(
                fields = List(
                  "decode" -> Typescript.Expression.Arrow(
                    arguments = List(Typescript.Expression.Symbol("value")),
                    body = Typescript.Expression.Call(
                      name = "String",
                      arguments = List(Typescript.Expression.Symbol("value"))
                    )
                  ),
                  "encode" -> Typescript.Expression.Arrow(
                    arguments = List(Typescript.Expression.Symbol("value")),
                    body = Typescript.Expression.Call(
                      name = "Number",
                      arguments = List(Typescript.Expression.Symbol("value"))
                    )
                  )
                )
              )
            )
          )
        ),
        Schema(
          Typescript.Expression.Call(
            name = "transform",
            arguments = List(
              Schema(Typescript.Expression.Symbol(name = "Boolean")),
              Schema(Typescript.Expression.Symbol(name = "String")),
              Typescript.Expression.Object(
                fields = List(
                  "decode" -> Typescript.Expression.Arrow(
                    arguments = List(Typescript.Expression.Symbol("value")),
                    body = Typescript.Expression.Ternary(
                      condition = Typescript.Expression.Symbol("value"),
                      valid = Typescript.Expression.Literal.String("true"),
                      invalid = Typescript.Expression.Literal.String("false")
                    )
                  ),
                  "encode" -> Typescript.Expression.Arrow(
                    arguments = List(Typescript.Expression.Symbol("value")),
                    body = Typescript.Expression.TripleEqual(
                      left = Typescript.Expression.Symbol("value"),
                      right = Typescript.Expression.Literal.String("true")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
