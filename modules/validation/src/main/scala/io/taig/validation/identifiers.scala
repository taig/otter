package io.taig.validation

object identifiers:
  inline def apply(inline value: String): Constraint.Identifier = Constraint.Identifier(value)

  val enumeration: Constraint.Identifier = collection("enumeration")
  val expected: Constraint.Identifier = collection("expected")
  val required: Constraint.Identifier = identifiers("required")
  val tpe: Constraint.Identifier = identifiers("type")

  object collection:
    inline def apply(inline name: String): Constraint.Identifier = identifiers(s"collection.$name")
    val atLeast: Constraint.Identifier = obj("at-least")
    val atMost: Constraint.Identifier = obj("at-most")
    val contains: Constraint.Identifier = obj("contains")
    val nonEmpty: Constraint.Identifier = collection("non-empty")

  object obj:
    inline def apply(inline name: String): Constraint.Identifier = identifiers(s"object.$name")
    val contains: Constraint.Identifier = obj("contains")

  object numeric:
    inline def apply(inline name: String): Constraint.Identifier = identifiers(s"numeric.$name")
    val equal: Constraint.Identifier = numeric("equal")
    val greaterThan: Constraint.Identifier = numeric("greater-than")
    val lessThan: Constraint.Identifier = numeric("less-than")

  object parser:
    inline def apply(inline name: String): Constraint.Identifier = identifiers(s"parser.$name")
    val uuid: Constraint.Identifier = parser("uuid")

  object text:
    inline def apply(inline name: String): Constraint.Identifier = identifiers(s"text.$name")
    val atLeast: Constraint.Identifier = text("at-least")
    val atMost: Constraint.Identifier = text("at-most")
    val email: Constraint.Identifier = text("email")
    val equal: Constraint.Identifier = text("equal")
    val exactly: Constraint.Identifier = text("exactly")
    val matches: Constraint.Identifier = text("matches")
    val required: Constraint.Identifier = text("required")
