package io.taig.otter.component

import io.taig.otter.Schema

trait NullableComponent[+Self[_], -Value[_]](using self: Schema.Nullable[Self, Value]):
  final def nullable[A](schema: => Value[A]): Self[Option[A]] = self.nullable(schema)
  final def nullable[A](schema: => Value[A], default: A): Self[A] = self.nullable(schema, default)
  final def void: Self[Unit] = self.void
