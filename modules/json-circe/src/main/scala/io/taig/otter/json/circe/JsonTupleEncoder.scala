// package io.taig.otter.json.circe

// import cats.syntax.all.*
// import io.circe.Json
// import cats.Id as Identity
// import io.taig.otter as Base
// import io.taig.otter.Plain.*

// object JsonTupleEncoder:
//   def apply[A](schema: Tuple.Writer[A], a: A): Option[Vector[Json]] = schema match
//     case Base.Tuple.Empty                       => Vector.empty.some
//     case Base.Tuple.Modify(self, _, f)          => modify(self, f, a)
//     case Base.Tuple.One(schema)                 => ??? // one(schema, a)
//     case Base.Tuple.Optional(self)              => optional(self, a)
//     case Base.Tuple.Writer.Empty                => Vector.empty.some
//     case Base.Tuple.Writer.Modify(self, f)      => modify(self, f, a)
//     case Base.Tuple.Writer.One(schema)          => ??? // one(schema, a)
//     case Base.Tuple.Writer.Optional(self)       => optional(self, a)
//     case Base.Tuple.Writer.Product(left, right) => product(left, right, a)
//     case Base.Tuple.Product(left, right)        => product(left, right, a)

//   def modify[A, B](self: Tuple.Writer[A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

//   def one[A](schema: Schema.Writer[A], a: A): Option[Vector[Json]] =
//     Vector(JsonEncoder(schema, a)).some

//   def optional[A](self: Tuple.Writer[A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

//   def product[A, B](left: Tuple.Writer[A], right: Tuple.Writer[B], ab: (A, B)): Option[Vector[Json]] =
//     (apply(left, ab._1), apply(right, ab._2)) match
//       case (Some(left), Some(right)) => Some(left ++ right)
//       case (Some(left), None)        => Some(left ++ Vector.fill(right.size)(Json.Null))
//       case (None, Some(right))       => Some(Vector.fill(left.size)(Json.Null) ++ right)
//       case (None, None)              => None
