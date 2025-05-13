package io.taig.otter

import cats.Invariant
// import cats.Eq
// import cats.syntax.all.*
// import io.taig.enumeration.ext.Mapping
// import io.taig.otter as Self

// import java.lang.String as JString
// import java.math.BigDecimal as JBigDecimal
// import java.math.BigInteger as JBigInteger
// import java.util.regex.Pattern
// import scala.Boolean as SBoolean
// import scala.Double as SDouble
// import scala.Float as SFloat
// import scala.Int as SInt
// import scala.Long as SLong

// trait Codec[Self[_]] extends Invariant[Self]:
//   extension [A](self: Self[A])
//     def metadata: Metadata
//     def modifyMetadata(f: Metadata => Metadata): Self[A]
//     final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
//     final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = modifyMetadata(_.put(key, value))

// object Codec:
//   object Extension:
//     trait Nullable[Self[_], Optional[_]](using optional: Codec.Nullable[Optional, Self]) extends Codec[Self]:
//       extension [A](self: Self[A])
//         final def nullable: Optional[Option[A]] = optional.nullable(self)
//         final def nullable(default: A): Optional[A] = optional.nullable(self, default)

//     trait Tupleable[Self[_], Tuple[_]](using tuple: Codec.Tuple[Tuple, Self])
//         extends Codec[Self], Product[Self, Self, Tuple]:
//       extension [A](self: Self[A])
//         final override def zip[B](codec: Self[B]): Tuple[(A, B)] =
//           tuple.one(self).zip(tuple.one(codec))

//     trait Unionable[Self[_], Union[_]](using union: Codec.Union[Union, Self])
//         extends Codec[Self],
//           Invariant.Coproduct[Self, Self, Union]:
//       extension [A](self: Self[A])
//         final override def orElse[B](codec: Self[B]): Union[Either[A, B]] =
//           union.one(self).orElse(union.one(codec))

//   trait Constant[Self[_], Value[_]] extends Codec[Self]:
//     def constant[A: Eq](codec: => Value[A], value: A): Self[Unit]

//   object Constant:
//     def apply[Self[_], Value[_]](
//         lift: [A] => (self: Self.Constant[Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Constant[Value, A]
//     ): Codec.Constant[Self, Value] = new Constant[Self, Value]:
//       override def constant[A](codec: => Value[A], value: A)(using eq: Eq[A]): Self[Unit] = lift(
//         Self.Constant
//           .Root(codec = Reference.Constant(self = Reference.later(codec), value), eq, metadata = Metadata.Empty)
//       )

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

//   trait Dictionary[Self[_], Key[_], Value[_]] extends Codec[Self]:
//     def dictionary[A, B](
//         key: => Key[A],
//         value: => Value[B],
//         minimum: Option[Int],
//         maximum: Option[Int]
//     ): Self[List[(A, B)]]

//   object Dictionary:
//     def apply[Self[_], Key[_], Value[_]](
//         lift: [A] => (self: Self.Dictionary[Key, Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Dictionary[Key, Value, A]
//     ): Codec.Dictionary[Self, Key, Value] = new Codec.Dictionary[Self, Key, Value]:
//       override def dictionary[A, B](
//           key: => Key[A],
//           value: => Value[B],
//           minimum: Option[Int],
//           maximum: Option[Int]
//       ): Self[List[(A, B)]] = lift(
//         Self.Dictionary.Root(
//           key = Reference.later(key),
//           value = Reference.later(value),
//           minimum,
//           maximum,
//           metadata = Metadata.Empty
//         )
//       )

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

//   trait Enumeration[Self[_], Value[_]] extends Codec[Self]:
//     def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B]

//   object Enumeration:
//     def apply[Self[_], Value[_]](
//         lift: [A] => (self: Self.Enumeration[Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Enumeration[Value, A]
//     ): Codec.Enumeration[Self, Value] = new Codec.Enumeration[Self, Value]:
//       override def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B] = lift(
//         Self.Enumeration.Root(codec = Reference.later(codec), mapping = mapping, metadata = Metadata.Empty)
//       )

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

//   trait Nullable[Self[_], Value[_]] extends Codec[Self]:
//     def nullable[A](codec: => Value[A]): Self[Option[A]]
//     def nullable[A](codec: => Value[A], default: A): Self[A]
//     def void: Self[Unit]

//   object Nullable:
//     def apply[Self[_], Value[_]](
//         lift: [A] => (self: Self.Nullable[Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Nullable[Value, A]
//     ): Codec.Nullable[Self, Value] = new Codec.Nullable[Self, Value]:
//       override def nullable[A](codec: => Value[A]): Self[Option[A]] = lift(
//         Self.Nullable.Root(reference = Reference.later(codec), metadata = Metadata.Empty)
//       )
//       override def nullable[A](codec: => Value[A], default: A): Self[A] = lift(
//         Self.Nullable.Default(reference = Reference.later(codec), default = default, metadata = Metadata.Empty)
//       )
//       override def void: Self[Unit] = lift(Self.Nullable.Void(metadata = Metadata.Empty))

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

//   trait Record[Self[_], Field[_]] extends Codec[Self], Invariant.Product[Self, Field, Self]:
//     final override def result: Invariant[Self] = this

//     def record[A](field: => Field[A]): Self[A]

//     extension [A](self: Self[A]) def optional: Self[Option[A]]

//   object Record:
//     def apply[Self[_], Field[_]](
//         lift: [A] => (self: Self.Record[Field, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Record[Field, A]
//     ): Codec.Record[Self, Field] = new Codec.Record[Self, Field]:
//       override def fromElement[A](codec: Field[A]): Self[A] = record(codec)

//       final override def record[A](field: => Field[A]): Self[A] =
//         lift(Self.Record.Root(field = Reference.later(field), metadata = Metadata.Empty))

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def optional: Self[Option[A]] = lift(extract(self).optional)
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
//         override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))

//   trait Sum[Self[_], Branch[_]] extends Codec[Self], Invariant.Coproduct[Self, Branch, Self] {}

//   object Sum:
//     def apply[Self[_], Branch[_]](
//         lift: [A] => (self: Self.Sum[Branch, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Sum[Branch, A]
//     ): Codec.Sum[Self, Branch] = new Codec.Sum[Self, Branch]:
//       override def result: Invariant[Self] = this
//       override def fromElement[A](codec: Branch[A]): Self[A] = ???

//       // final override def one[A](codec: => Branch[A]): Self[A] =
//       //   lift(Self.Sum.Root(branch = Reference.later(codec), discriminator = ???, metadata = Metadata.Empty))

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
//         override def orElse[B](codec: Self[B]): Self[Either[A, B]] = lift(extract(self).orElse(extract(codec)))

//   trait Tuple[Self[_], Value[_]] extends Codec[Self], Invariant.Product[Self, Value, Self]:
//     final override def result: Invariant[Self] = this
//     final override def fromElement[A](codec: Value[A]): Self[A] = one(codec)

//     def empty: Self[Unit]
//     def one[A](codec: => Value[A]): Self[A]

//   object Tuple:
//     def apply[Self[_], Value[_]](
//         lift: [A] => (self: Self.Tuple[Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Tuple[Value, A]
//     ): Codec.Tuple[Self, Value] = new Codec.Tuple[Self, Value]:
//       override val empty: Self[Unit] = lift(Self.Tuple.Empty(metadata = Metadata.Empty))

//       override def one[A](codec: => Value[A]): Self[A] =
//         lift(Self.Tuple.Root(codec = Reference.later(codec), metadata = Metadata.Empty))

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
//         override def zip[B](codec: Self[B]): Self[(A, B)] = lift(extract(self).zip(extract(codec)))

//   trait Union[Self[_], Value[_]] extends Codec[Self], Invariant.Coproduct[Self, Value, Self]:
//     final override def result: Invariant[Self] = this
//     final override def fromElement[A](codec: Value[A]): Self[A] = one(codec)

//     def one[A](codec: => Value[A]): Self[A]

//   object Union:
//     def apply[Self[_], Value[_]](
//         lift: [A] => (self: Self.Union[Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Union[Value, A]
//     ): Codec.Union[Self, Value] = new Codec.Union[Self, Value]:
//       override def one[A](codec: => Value[A]): Self[A] =
//         lift(Self.Union.Root(codec = Reference.later(codec), metadata = Metadata.Empty))

//       extension [A](self: Self[A])
//         override def metadata: Metadata = extract(self).metadata
//         override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//         override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
//         override def orElse[B](codec: Self[B]): Self[Either[A, B]] = lift(extract(self).orElse(extract(codec)))

//   trait Field[Self[_], -Key[_], -Value[_], Record[_]](using record: Invariant[Record])
//       extends Codec[Self],
//         Invariant.Product[Self, Self, Record]:
//     final override def result: Invariant[Record] = record

//     def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

//     extension [A](self: Self[A])
//       def optional: Self[Option[A]]
//       def toRecord: Record[A]

//   object Field:
//     def apply[Self[_], Key[_], Value[_], Record[_]: Invariant](
//         lift: [A] => (self: Self.Field[Key, Value, A]) => Self[A],
//         extract: [A] => (self: Self[A]) => Self.Field[Key, Value, A]
//     )(using codec: Codec.Record[Record, Self]): Codec.Field[Self, Key, Value, Record] =
//       new Codec.Field[Self, Key, Value, Record]:
//         final override inline def fromElement[A](codec: Self[A]): Self[A] = codec
//         override def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B] = lift:
//           Self.Field.Root(
//             key = Reference.Constant(self = Reference.later(key), value = name),
//             value = Reference.later(value),
//             metadata = Metadata.Empty
//           )

//         extension [A](self: Self[A])
//           override def metadata: Metadata = extract(self).metadata
//           override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
//           override def optional: Self[Option[A]] = lift(extract(self).optional)
//           override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
//           override def zip[B](codec: Self[B]): Record[(A, B)] = toRecord.zip(codec.toRecord)
//           override def toRecord: Record[A] = codec.record(self)
