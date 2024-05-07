package io.taig.otter

import io.taig.otter.Schema.Read
import io.taig.otter.validation.Validation

// sealed abstract class Primitive[A] extends Schema[Nothing, A], Primitive.Read[A], Primitive.Write[A]:
//   override def asRead: Primitive.Read[A]
//   override def asWrite: Primitive.Write[A]

//   override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(f: C => A): Primitive[C] =
//     Primitive.Optional(asRead.validate(constraint)(validation), asWrite.contramap(f))

//   override def optional: Primitive[Option[A]] = Primitive.Optional(asRead.optional, asWrite.optional)

// object Primitive:
//   trait Operation:
//     def tpe: Type[?]

//   final case class Required[A] private (asRead: Primitive.Required.Read[A], asWrite: Primitive.Required.Write[A])
//       extends Primitive[A],
//         Primitive.Required.Read[A],
//         Primitive.Required.Write[A]:
//     override def ivalidate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C])(
//         f: C => A
//     ): Primitive.Required[C] = Required(asRead.validate(constraint)(validation), asWrite.contramap(f))
//     override def optional: Primitive[Option[A]] = Primitive.Optional(asRead.optional, asWrite.optional)

//   object Required:
//     sealed trait Read[+A] extends Primitive.Read[A]:
//       final override def validate[B, C](constraint: Schema[?, B])(
//           validation: Validation[A, B, C]
//       ): Primitive.Required.Read[C] = Read.Validate(this, constraint, validation)
//       override def optional: Primitive.Read[Option[A]] = Read.Optional(this)

//     object Read:
//       final case class Validate[A, B, C](
//           self: Primitive.Required.Read[A],
//           constraint: Schema[?, B],
//           validation: Validation[A, B, C]
//       ) extends Primitive.Required.Read[C]

//       final case class Optional[A](self: Primitive.Required.Read[A]) extends Primitive.Required.Read[Option[A]]

//     sealed trait Write[-A] extends Primitive.Write[A]:
//       final override def contramap[B](f: B => A): Primitive.Required.Write[B] = Write.Modify(this, f)
//       override def optional: Primitive.Write[Option[A]] = Write.Optional(this)

//     object Write:
//       final case class Modify[A, B](self: Primitive.Required.Write[A], f: B => A) extends Primitive.Required.Write[B]

//       final case class Optional[A](self: Primitive.Required.Write[A]) extends Primitive.Required.Write[Option[A]]

//   final case class Optional[A] private[otter] (asRead: Primitive.Read[A], asWrite: Primitive.Write[A])
//       extends Primitive[A]

//   sealed trait Read[+A] extends Schema.Read[Nothing, A]:
//     override def validate[B, C](constraint: Schema[?, B])(validation: Validation[A, B, C]): Primitive.Read[C] = ???

//     override def optional: Primitive.Read[Option[A]] = ???

//   sealed trait Write[-A] extends Schema.Write[Nothing, A]:
//     override def contramap[B](f: B => A): Primitive.Write[B] = ???
//     override def optional: Primitive.Write[Option[A]] = ???
