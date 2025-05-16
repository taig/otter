package io.taig.otter.component

import io.taig.otter.schema.SumSchema

trait SumComponent[Self[_], -Branch[_]](self: SumSchema[Self, Branch]):
  extension [A](self: Self[A])
    final def orElse[B](schema: Self[B]): Self[Either[A, B]] = this.self.orElse(self)(schema)
    final def :+[B](branch: Branch[B]): Self[Either[A, B]] = orElse(this.self.lift(branch))

  extension [A](branch: Branch[A])
    def *:[B](schema: Self[B]): Self[Either[A, B]] = this.self.lift(branch).orElse(schema)

  extension [A](self: Self[A]) final def |[B](branch: Branch[B]): Self[A | B] = ???
