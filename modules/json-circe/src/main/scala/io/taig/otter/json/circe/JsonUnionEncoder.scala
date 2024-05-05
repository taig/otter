// package io.taig.otter.json.circe

// import io.taig.otter.+
// import io.taig.otter.Union
// import io.taig.otter.Schema
// import io.circe.Json
// import io.taig.otter.Union.One
// import io.taig.otter.Union.OrElse
// import io.taig.otter.Union.Optional
// import io.taig.otter.Union.Validate

// object JsonUnionEncoder:
//   def encode[A](schema: Union[Schema[?], A], a: A): Json = schema match
//     case Union.One(schema)               => JsonEncoder.encode(schema, a)
//     case Union.OrElse(left, right)       => encode(left, right, a)
//     case Union.Optional(schema)          => a.map(encode(schema, _)).getOrElse(Json.Null)
//     case Union.Validate(schema, _, _, g) => encode(schema, g(a))

//   def encode[A, B](left: Union[Schema[?], A], right: Union[Schema[?], B], ab: A + B): Json = ab match
//     case Left(a)  => encode(left, a)
//     case Right(b) => encode(right, b)
