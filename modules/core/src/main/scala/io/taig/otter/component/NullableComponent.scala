package io.taig.otter.component

import io.taig.otter.schema.NullableSchema

trait NullableComponent[+Self[_], -Value[_]](using self: NullableSchema[Self, Value]):
  final def void: Self[Unit] = self.void

  extension [A](value: Value[A])
    final def nullable: Self[Option[A]] = this.self(schema = value)
    final def nullable(default: A): Self[A] = this.self(schema = value, default)
