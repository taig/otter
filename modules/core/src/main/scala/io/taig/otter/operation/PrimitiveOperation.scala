// package io.taig.otter.operation

// import cats.data.Chain
// import io.taig.otter.Constraint

// trait PrimitiveOperation[Self[_]] extends BooleanOperation[Self], NumberOperation[Self], StringOperation[Self]:
//   override def constraints[A](self: Self[A]): Chain[Constraint.Primitive.Number | Constraint.Primitive.Text] = ???

// object PrimitiveOperation:
//   inline def apply[Self[_]](using operation: PrimitiveOperation[Self]): PrimitiveOperation[Self] = operation
