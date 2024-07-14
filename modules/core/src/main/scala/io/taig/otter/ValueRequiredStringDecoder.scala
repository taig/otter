package io.taig.otter

object ValueRequiredStringDecoder:
  def apply[A](schema: Value.Required.Via[String, A], value: String): Decoder.Result[Option[String], A] =
    schema match
      case schema: Enumeration.Required.Via[String, A] => EnumerationRequiredStringDecoder(schema, value)
      case schema: Primitive.Required[A]               => PrimitiveRequiredStringDecoder(schema, value)
      case schema: Union.Value.Required.Via[String, A] => UnionValueRequiredStringDecoder(schema, value)
