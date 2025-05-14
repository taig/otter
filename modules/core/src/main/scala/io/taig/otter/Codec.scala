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
