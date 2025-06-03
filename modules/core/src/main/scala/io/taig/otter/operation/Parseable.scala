package io.taig.otter.operation

trait Parseable[-Self[_], +String[_]]:
  extension [A](schema: => Self[A]) def parsed: String[A]

object Parseable:
  def apply[Self[_], String[_]](using self: PrimitiveSchemaInvariant.String[String, Self]): Parseable[Self, String] =
    new Parseable[Self, String]:
      extension [A](schema: => Self[A]) override def parsed: String[A] = self.parsed(schema)
