package io.taig.otter.component

import io.taig.otter.schema.NullableSchema

trait NullableComponent[+Self[_], -Value[_]](using self: NullableSchema[Self, Value]):
  final def nullable[A](schema: => Value[A]): Self[Option[A]] = self(schema)
  final def nullable[A](schema: => Value[A], default: A): Self[A] = self(schema, default)
  final def void: Self[Unit] = self.void
