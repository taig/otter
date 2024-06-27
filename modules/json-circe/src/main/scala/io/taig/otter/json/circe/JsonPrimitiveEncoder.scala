// package io.taig.otter.json.circe

// import io.circe.Json
// import io.taig.otter as Base
// import io.taig.otter.Plain.*

// object JsonPrimitiveEncoder:
//   def apply[A](schema: Primitive.Writer[A], a: A): Json = schema match
//     case Base.Primitive.Optional(self)                  => optional(self, a)
//     case Base.Primitive.Required.Root(_, tpe)           => JsonTypeEncoder(tpe, a)
//     case Base.Primitive.Required.Validate(self, _, f)   => modify(self, f, a)
//     case Base.Primitive.Required.Writer.Modify(self, f) => modify(self, f, a)
//     case Base.Primitive.Validate(self, _, f)            => modify(self, f, a)
//     case Base.Primitive.Writer.Modify(self, f)          => modify(self, f, a)
//     case Base.Primitive.Writer.Optional(self)           => optional(self, a)

//   def modify[A, B](self: Primitive.Writer[A], f: B => A, b: B): Json = apply(self, f(b))

//   def optional[A](self: Primitive.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)
