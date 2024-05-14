// package io.taig.otter.json.circe

// import cats.data.Chain
// import cats.syntax.all.*
// import io.circe.Json
// import io.taig.otter as Base
// import io.taig.otter.Plain.*

// object JsonTupleEncoder:
//   def apply[A](schema: Tuple.Writer[A], a: A): Option[Chain[Json]] = schema match
//     case Base.Tuple.Empty                => Chain.empty.some
//     case Base.Tuple.One(schema)          => Chain.one(JsonEncoder(schema, a)).some
//     case Base.Tuple.Optional(self)       => a.flatMap(apply(self, _))
//     case Base.Tuple.Product(left, right) => apply(left, a._1) |+| apply(right, a._2)
