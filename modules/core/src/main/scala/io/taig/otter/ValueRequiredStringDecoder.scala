package io.taig.otter

object ValueRequiredStringDecoder:
  def apply[A](schema: Value.Required.Reader.Via[String, A], value: String): Decoder.Result[Option[String], A] =
    schema match
      case schema: Enumeration.Required.Reader.Via[String, A] => EnumerationRequiredStringDecoder(schema, value)
      case schema: Primitive.Required.Reader[A]               => PrimitiveRequiredStringDecoder(schema, value)
      case schema: Union.Value.Required.Reader.Via[String, A] => UnionValueRequiredStringDecoder(schema, value)
