package io.taig.otter

import io.taig.otter as Base
import cats.Id as Identity
import cats.Applicative
import cats.Comonad

trait Ops extends Instances
// trait SchemaOps[Self[_, _, _], Optional[_, _, _], Collection[_, _, _], Union[_, _, _], Plain[_, _]]

// object SchemaOps:
//   trait Isomorphic[Self[a, b, c] <: Optional[a, b, c], Optional[a, b, c] <: Schema.With[a, b, c]]
//       extends SchemaOps[Self, Optional, Collection.With, Union.With, Base.Schema[Identity, *, ?, *]]:
//     extension [A, B, C](self: Self[A, B, C])
//       final def collection: Collection.With[A, self.type, Vector[C]] =
//         Applicative[container.Collection].pure(Base.Collection.Root(self))
//       final def union: Union.With[A, self.type, C] =
//         Applicative[container.Union].pure(Base.Union.Root(self))
//       def optional: Optional[A, B, Option[C]]
//       def plain: Base.Schema[Identity, A, ?, C]

// trait PrimitiveOps[
//     Self[_] <: container.Primitive[Base.Primitive.Ops],
//     Optional[_],
//     Collection[_, _, _],
//     Union[_, _, _],
//     Plain[_]
// ] extends Base.PrimitiveOps[Self, Optional, Collection, Union, Plain]:
//   extension [A](self: Self[A]) final override def tpe: Type[?] = Comonad[container.Primitive].extract(self).tpe

// object PrimitiveOps:
//   trait Isomorphic[Self[a] <: Optional[a], Optional[a] <: Primitive[a]]
//       extends PrimitiveOps[Self, Optional, Collection.With, Union.With, Base.Primitive]

//   trait Reader[Self[_, _, _], Optional[_, _, _]]:
//     extension [A, B, C](self: Self[A, B, C])
//       final def collection: Collection.Reader.With[A, self.type, Vector[C]] =
//         Applicative[container.Collection].pure(???)
//       final def union: Union.Reader.With[A, self.type, C] = ???
//       def optional: Optional[A, B, Option[C]]
//       def toPlain: Base.Schema.Reader[Identity, A, ?, C]

//   trait Writer[Self[_, _, _], Optional[_, _, _]]:
//     extension [A, B, C](self: Self[A, B, C])
//       final def collection: Collection.Writer.With[A, self.type, Vector[C]] =  Applicative[container.Collection].pure(???)
//       final def optional: Optional[A, B, Option[C]] = ???
//       def union: Union.Writer.With[A, self.type, C]
//       def toPlain: Base.Schema.Writer[Identity, A, ?, C]

// trait PrimitiveOps[Self[a] <: Optional[a], Optional[a] <: Primitive[a]]
//     extends SchemaOps[[_, _, a] =>> Self[a], [_, _, a] =>> Optional[a]]:
//   extension [A](self: Self[A])
//       def tpe: Type[?]

//   extension [A, B, C](self: Self[C])
//       override def plain: Base.Primitive[C]

// object PrimitiveOps:
//   trait Reader[Self[_], Optional[_]] extends SchemaOps.Reader[[_, _, a] =>> Self[a], [_, _, a] =>> Optional[a]]:
//     extension [A](self: Self[A])
//       def tpe: Type[?]

//     extension [A, B, C](self: Self[C])
//       override def toPlain: Base.Primitive.Reader[C]

//   trait Writer[Self[_], Optional[_]] extends SchemaOps.Writer[[_, _, a] =>> Self[a], [_, _, a] =>> Optional[a]]:
//     extension [A](self: Self[A])
//       def tpe: Type[?]

//     extension [A, B, C](self: Self[C])
//       override def toPlain: Base.Primitive.Writer[C]

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
