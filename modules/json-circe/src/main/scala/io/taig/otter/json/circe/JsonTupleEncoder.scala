package io.taig.otter.json.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Fix
import io.taig.otter.Schema
import io.taig.otter.Tuple
import io.taig.otter.Cofree
import io.taig.otter.TupleArrow

// object JsonTupleEncoder:
//   def apply[A, B](schema: Tuple[A, B], b: B): Option[Chain[Json]] = schema match
//     case Tuple.Product(left, right) =>
//       apply(left, b._1)
//       apply(right, b._2)
//       ???
//     case Tuple.Empty => Some(Chain.empty)
//     case Tuple.One(schema, unwrap) =>
//       Some(Chain.one(JsonEncoder(unwrap(schema), b)))

final case class Annotation[+S, +M](self: S, metadata: M)

// object Playground:
//   type AnnotatedSchema[+A, B] = Annotation[Schema[A, B], Unit]
//   type AnnotatedTuple[+A, B] = Annotation[Tuple[A, B], Unit]

//   given TupleArrow[AnnotatedTuple] = new TupleArrow[AnnotatedTuple] {
//     extension [A, B](self: AnnotatedTuple[A, B])
//       override def optional: AnnotatedTuple[A, Option[B]] =
//         Annotation(self.self.optional, ())

//     extension [A, B](self: AnnotatedTuple[A, B]) override def schemas: Chain[A] = self.self.schemas

//     extension [A, B](self: AnnotatedTuple[A, B])
//       override def product[C, D](tuple: AnnotatedTuple[C, D]): AnnotatedTuple[A | C, (B, D)] =
//         val r = self.self.product(tuple.self)
//         Annotation(r, ())

//   }

//   val x: AnnotatedTuple[Fix[[x] =>> Annotation[Schema[x, ?], Unit]], (String, Int)] = ???

//   val z: AnnotatedTuple[Fix[[x] =>> Annotation[Schema[x, ?], Unit]], ((String, Int), (String, Int))] = x.product(x)

// object JsonTupleEncoder:
//   def apply[A, B](schema: Tuple.Writer[JsonSchema.Writer[A], B], b: B): Option[Chain[Json]] = schema match
//     case Tuple.Writer.Empty | Tuple.Empty  => Chain.empty.some
//     case Tuple.One(schema)                 => Chain.one(JsonEncoder(schema, b)).some
//     case Tuple.Writer.One(schema)          => Chain.one(JsonEncoder(schema, b)).some
//     case Tuple.Optional(schema)            => b.flatMap(apply(schema, _))
//     case Tuple.Writer.Optional(schema)     => b.flatMap(apply(schema, _))
//     case Tuple.Validate(schema, _, f)      => apply(schema, f(b))
//     case Tuple.Writer.Modify(schema, f)    => apply(schema, f(b))
//     case Tuple.Product(left, right)        => apply(left, right, b)
//     case Tuple.Writer.Product(left, right) => apply(left, right, b)

//   def apply[A, B, C](
//       left: Tuple.Writer[JsonSchema.Writer[A], B],
//       right: Tuple.Writer[JsonSchema.Writer[A], C],
//       ab: (B, C)
//   ): Option[Chain[Json]] = (apply(left, ab._1), apply(right, ab._2)) match
//     case (Some(left), Some(right)) => (left ++ right).some
//     case (Some(left), None)        => (left ++ right.schemas.as(Json.Null)).some
//     case (None, Some(right))       => (left.schemas.as(Json.Null) ++ right).some
//     case (None, None)              => none
