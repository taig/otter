// package io.taig.otter

// import io.taig.otter.Plain.*
// import io.taig.otter.Decoder.Result

// object ValueRequiredStringDecoder:
//   def apply[A](schema: Value.Required.Reader[A], value: String): Decoder.Result[Option[String], A] = schema match
//     case schema: Enumeration.Required.Reader[A] => EnumerationRequiredStringDecoder(schema, value)
//     case schema: Primitive.Required.Reader[A]   => PrimitiveRequiredStringDecoder(schema, value)
//     case schema: Union.Value.Required.Reader[A] => UnionValueRequiredStringDecoder(schema, value)
