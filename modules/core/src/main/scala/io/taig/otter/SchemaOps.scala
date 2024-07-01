package io.taig.otter

import cats.Id as Identity

trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]:
  extension [A, B](self: Self[A, B])
    def collection: Collection[self.type, Vector[B]]
    def optional: Optional[A, Option[B]]
    def union: Union[self.type, B]

object SchemaOps:
  trait Isomorphic[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B]) def toPlain: Schema[Identity, ?, B]

  trait Reader[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B])
      final def asReader: Self[A, B] = self
      def toPlain: Schema.Reader[Identity, ?, B]

  trait Writer[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
      extends SchemaOps[Self, Optional, Collection, Union]:
    extension [A, B](self: Self[A, B])
      final def asWriter: Self[A, B] = self
      def toPlain: Schema.Writer[Identity, ?, B]
      final def toValidationWriter(value: B): ValidationWriter[B] = ValidationWriter(self.toPlain, value)
