package io.taig.otter.component

import cats.syntax.all.*
import io.taig.otter.schema.Schema

import scala.annotation.targetName

trait SchemaComponent[Self[_]](using self: Schema[Self]):
  extension (self: Self[Unit])
    final def as[A](a: A): Self[A] = self.imap(_ => a)(_ => ())

    @targetName("asSingleton")
    final def as[A <: Singleton](a: A): Self[A] = self.imap(_ => a)(_ => ())
