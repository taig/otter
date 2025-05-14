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
