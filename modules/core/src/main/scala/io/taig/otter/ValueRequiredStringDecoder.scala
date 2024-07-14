package io.taig.otter

object ValueRequiredStringDecoder:
  def apply[A](schema: Value.Required[?, A], value: String): Decoder.Result[Option[String], A] =
    schema match
      case schema: Enumeration.Required[?, A] => EnumerationRequiredStringDecoder(schema, value)
      case schema: Primitive.Required[A]      => PrimitiveRequiredStringDecoder(schema, value)
      case schema: Union.Value.Required[?, A] => UnionValueRequiredStringDecoder(schema, value)
