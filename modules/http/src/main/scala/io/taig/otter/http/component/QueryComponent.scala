// package io.taig.otter.http.component

// import io.taig.otter.http.Http
// import io.taig.otter.http.operation.QueryOperation
// import io.taig.otter.Reference
// import io.taig.otter.component.PrimitiveComponent

// trait QueryComponent[F[_], G[_]](using F: QueryOperation[F, G]):
//   def apply[A](name: String, parameter: => G[A]): F[A] =
//     F.lift(name, parameter = Reference.later(parameter))

// object QueryComponent:
//   trait Parameter
//       extends PrimitiveComponent.Boolean[Http.Query.Parameter.Primitive.Boolean],
//         PrimitiveComponent.Number[Http.Query.Parameter.Primitive.Number],
//         PrimitiveComponent.Text[Http.Query.Parameter.Primitive.Text]
