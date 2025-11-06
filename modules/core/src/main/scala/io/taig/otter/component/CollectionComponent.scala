package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import io.taig.otter.Reference
import io.taig.otter.:<:
import cats.data.Chain
import io.taig.otter.Schema
import io.taig.otter.operation.PrimitiveOperation

trait CollectionComponent[Self[_[_], _], Shape[_]](using operation: CollectionOperation.Aux[Self, Shape]):
  def chain[S[_], A](schema: => S[A], validation: Validation[Constraint.Collection, Chain[A]])(using
      S :<: Shape
  ): Self[S, Chain[A]] = operation.chained(schema = Reference.later(schema), validation)

trait PrimitiveComponent[Self[_]](using operation: PrimitiveOperation[Self]):
  def string: Self[String] = operation.string(Validation.valid)

// object SchemaComponent
//     extends CollectionComponent[Schema.Collection, Schema[?, *]],
//       PrimitiveComponent[Schema.Primitive]

// object Playground:
//   import SchemaComponent.*

//   chain(string, Validation.valid)
