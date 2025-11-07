package io.taig.otter.component

import io.taig.otter as Self
import io.taig.otter.Collection
import io.taig.validation.Validation
import io.taig.otter.Constraint
import io.taig.otter.Reference

// trait CollectionComponent[F[+_[a] <: G[a], _], G[_]](using val F: Collection[F, G]):
//   object collection:
//     def list[H[a] <: G[a], A](schema: => H[A], validation: Validation[Constraint.Collection, List[A]]): F[H, List[A]] =
//       F.linked(schema = Reference.later(schema), validation)

//     def list[H[a] <: G[a], A](schema: => H[A]): F[H, List[A]] = list(schema, validation = Validation.valid)

// object JsonComponent extends CollectionComponent[Json.Collection.Of, Json]

// object Playground:
//   import JsonComponent.*

//   val name: Json[String] = ???

//   val names: Json.Collection[List[String]] = collection.list(name)
