package io.taig.otter

import io.taig.otter as Base
import cats.Applicative
import cats.Id as Identity
import cats.Comonad

trait Ops extends Instances:
  trait SchemaOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[_]]:
    extension [A, B](self: Self[A, B])
      def collection: Collection[self.type, Vector[B]]
      def optional: Optional[A, Option[B]]
      def union: Union[self.type, B]
      def toPlain: Plain[B]

  // object SchemaOps:
  //   trait Isomorphic[Self[a, b] <: Schema.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema[Identity, ?, a]]
  //       extends SchemaOps[Self, Optional, Collection.Of, Union.Of, Plain]:
  //     extension [A, B](self: Self[A, B])
  //       final override def collection: Collection.Of[self.type, Vector[B]] =
  //         Applicative[container.Collection].pure(Base.Collection.Root(self))
  //       final override def union: Union.Of[self.type, B] = Applicative[container.Union].pure(Base.Union.Root(self))

  //   trait Reader[Self[a, b] <: Schema.Reader.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema.Reader[Identity, ?, a]]
  //       extends SchemaOps[Self, Optional, Collection.Reader.Of, Union.Reader.Of, Plain]:
  //     extension [A, B](self: Self[A, B])
  //       final override def collection: Collection.Reader.Of[self.type, Vector[B]] =
  //         Applicative[container.Collection].pure(Base.Collection.Reader.Root(self))
  //       final override def union: Union.Reader.Of[self.type, B] =
  //         Applicative[container.Union].pure(Base.Union.Reader.Root(self))

  //   trait Writer[Self[a, b] <: Schema.Writer.Of[a, b], Optional[_, _], Plain[a] <: Base.Schema.Writer[Identity, ?, a]]
  //       extends SchemaOps[Self, Optional, Collection.Writer.Of, Union.Writer.Of, Plain]:
  //     extension [A, B](self: Self[A, B])
  //       final override def collection: Collection.Writer.Of[self.type, Vector[B]] =
  //         Applicative[container.Collection].pure(Base.Collection.Writer.Root(self))
  //       final override def union: Union.Writer.Of[self.type, B] =
  //         Applicative[container.Union].pure(Base.Union.Writer.Root(self))
  //       def toValidationWriter: B => ValidationWriter[B] = ValidationWriter.Root(self.toPlain, _)

//   trait ValueOps[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _], Plain[_]]
//       extends SchemaOps[Self, Optional, Collection, Union, Plain]

//   object ValueOps:
//     trait Isomorphic[Self[a, b] <: Value.Of[a, b], Optional[_, _], Plain[a] <: Base.Value[Identity, ?, a]]
//         extends ValueOps[Self, Optional, Collection.Of, Union.Of, Plain],
//           SchemaOps.Isomorphic[Self, Optional, Plain]

//     trait Reader[Self[a, b] <: Value.Reader.Of[a, b], Optional[_, _], Plain[a] <: Base.Value.Reader[Identity, ?, a]]
//         extends ValueOps[Self, Optional, Collection.Reader.Of, Union.Reader.Of, Plain],
//           SchemaOps.Reader[Self, Optional, Plain]

//     trait Writer[Self[a, b] <: Value.Writer.Of[a, b], Optional[_, _], Plain[a] <: Base.Value.Writer[Identity, ?, a]]
//         extends ValueOps[Self, Optional, Collection.Writer.Of, Union.Writer.Of, Plain],
//           SchemaOps.Writer[Self, Optional, Plain]:
//       extension [A, B](self: Self[A, B])
//         final override def toValidationWriter: B => ValidationWriter.Value[B] =
//           ValidationWriter.Value.Root(self.toPlain, _)

//   trait CollectionOps[Self[_, _], Union[_, _], Plain[_], Any] extends SchemaOps[Self, Self, Self, Union, Plain]:
//     extension [A, B](self: Self[A, B]) def schema: Any

//   object CollectionOps:
//     trait Isomorphic extends CollectionOps[Collection.Of, Union.Of, Base.Collection[Identity, ?, *], Schema.Any]:
//       extension [A, B](self: Collection.Of[A, B])
//         override def schema: Schema.Any = Comonad[container.Collection].extract(self).schema

//     trait Reader
//         extends CollectionOps[
//           Collection.Reader.Of,
//           Union.Reader.Of,
//           Base.Collection.Reader[Identity, ?, *],
//           Schema.Reader.Any
//         ]:
//       extension [A, B](self: Collection.Reader.Of[A, B])
//         override def schema: Schema.Reader.Any = Comonad[container.Collection].extract(self).schema

//     trait Writer
//         extends CollectionOps[
//           Collection.Writer.Of,
//           Union.Writer.Of,
//           Base.Collection.Writer[Identity, ?, *],
//           Schema.Writer.Any
//         ]:
//       extension [A, B](self: Collection.Writer.Of[A, B])
//         override def schema: Schema.Writer.Any = Comonad[container.Collection].extract(self).schema

//   trait PrimitiveOps[Self[_], Optional[_], Collection[_, _], Union[_, _], Plain[_]]
//       extends ValueOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union, Plain]:
//     extension [A](self: Self[A]) def tpe: Type[?]

//   object PrimitiveOps:
//     trait Isomorphic[Self[a] <: Primitive[a], Optional[_], Plain[a] <: Base.Value[Identity, ?, a]]
//         extends PrimitiveOps[Self, Optional, Collection.Of, Union.Of, Plain],
//           ValueOps.Isomorphic[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]

//     trait Reader[Self[a] <: Primitive.Reader[a], Optional[_], Plain[a] <: Base.Value.Reader[Identity, ?, a]]
//         extends PrimitiveOps[Self, Optional, Collection.Reader.Of, Union.Reader.Of, Plain],
//           ValueOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]

//     trait Writer[Self[a] <: Primitive.Writer[a], Optional[_], Plain[a] <: Base.Value.Writer[Identity, ?, a]]
//         extends PrimitiveOps[Self, Optional, Collection.Writer.Of, Union.Writer.Of, Plain],
//           ValueOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Plain]
