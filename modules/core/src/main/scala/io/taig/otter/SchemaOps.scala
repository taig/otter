package io.taig.otter

import cats.Id as Identity

trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[_]]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[self.type, Vector[B]]
    def optional: Optional[A, Option[B]]
    def union: Union[self.type, B]
    def toPlain: Plain[B]

object SchemaOps:
  trait Isomorphic[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[a] <: Schema[Identity, ?, a]]
      extends SchemaOps[Self, Optional, Collection, Union, Plain]

  trait Reader[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[a] <: Schema.Reader[Identity, ?, a]]
      extends SchemaOps[Self, Optional, Collection, Union, Plain]:
    extension [A, B](self: Self[A, B]) final def asReader: Self[A, B] = self

  trait Writer[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[a] <: Schema.Writer[Identity, ?, a]]
      extends SchemaOps[Self, Optional, Collection, Union, Plain]:
    extension [A, B](self: Self[A, B])
      final def asWriter: Self[A, B] = self
      final def toValidationWriter(value: B): ValidationWriter[B] = ValidationWriter(self.toPlain, value)
