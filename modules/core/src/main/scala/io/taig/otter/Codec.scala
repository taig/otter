package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import cats.data.NonEmptyChain
import io.taig.otter.validation.Validation
import io.taig.otter as Base
import io.taig.otter
import io.taig.enumeration.ext.Mapping
import cats.data.Validated
import Base.validation.Violations
import Base.validation.Violation

abstract class Codec[+O, A]:
  def metadata: Metadata
  def imap[B](f: A => B)(g: B => A): Codec[O, B]
  def optional: Codec[O, Option[A]]
  def update(f: Metadata => Metadata): Codec[O, A]

  final def decode(data: Data): Codec.Result[Data, A] = decodeOption(data.toValue)
  def decodeOption(data: Option[Data.Value]): Codec.Result[Data, A]

  final def encode(a: A): Data = encodeOption(a).getOrElse(Data.Null)
  def encodeOption(a: A): Option[Data.Value]

object Codec:
  type Result[A, B] = Validated[Violations[Violation[Constraint.Any[A], A]], B]

// sealed trait Enumeration[+O, A] extends Value[O, A]:
//   override def imap[B](f: A => B)(g: B => A): Enumeration[O, B] = Enumeration.Transform(this, f, g)
//   override def optional: Enumeration[O, Option[A]] = Enumeration.Optional(this)
//   override def update(f: Metadata => Metadata): Enumeration[O, A]

// object Enumeration:
//   sealed trait Required[+B, C] extends Value.Required[B, C], Enumeration[B, C]:
//     override def imap[D](f: C => D)(g: D => C): Enumeration.Required[B, D] = Required.Transform(this, f, g)
//     override def update(f: Metadata => Metadata): Enumeration.Required[B, C]

//   object Required:
//     final case class Root[O, A, B](metadata: Metadata, schema: Value.Required[O, A], mapping: Mapping[B, A])
//         extends Enumeration.Required[O, B]:
//       override def update(f: Metadata => Metadata): Enumeration.Required[O, B] = copy(metadata = f(metadata))

//     final case class Transform[B, C, D](self: Enumeration.Required[B, C], f: C => D, g: D => C)
//         extends Enumeration.Required[B, D]:
//       export self.metadata
//       override def update(f: Metadata => Metadata): Enumeration.Required[B, D] = copy(self = self.update(f))

//   final case class Optional[B, C](self: Enumeration[B, C]) extends Enumeration[B, Option[C]]:
//     export self.metadata
//     override def update(f: Metadata => Metadata): Enumeration[B, Option[C]] = copy(self = self.update(f))

//   final case class Root[O <: Value[?, A], A, B](metadata: Metadata, schema: O, mapping: Mapping[B, A])
//       extends Enumeration[O, B]:
//     override def update(f: Metadata => Metadata): Enumeration[O, B] = copy(metadata = f(metadata))

//   final case class Transform[B, C, D](self: Enumeration[B, C], f: C => D, g: D => C) extends Enumeration[B, D]:
//     export self.metadata
//     override def update(f: Metadata => Metadata): Enumeration[B, D] = copy(self = self.update(f))

// sealed trait Primitive[A] extends Value[Nothing, A]:
//   def constraints: Chain[Constraint.Primitive[?]]
//   override def imap[B](f: A => B)(g: B => A): Primitive[B] = ivalidate(Validation.lift(f))(g)
//   def ivalidate[B](validation: SchemaValidation.Primitive[A, B])(f: B => A): Primitive[B] =
//     Primitive.Transform(this, validation, f)
//   final override def optional: Primitive[Option[A]] = Primitive.Optional(this)
//   def tpe: Type[?]
//   override def update(f: Metadata => Metadata): Primitive[A]

// object Primitive:
//   sealed trait Required[A] extends Value.Required[Nothing, A], Primitive[A]:
//     final override def imap[C](f: A => C)(g: C => A): Primitive.Required[C] = ivalidate(Validation.lift(f))(g)
//     override def ivalidate[B](validation: SchemaValidation.Primitive[A, B])(f: B => A): Primitive.Required[B] =
//       Required.Transform(this, validation, f)
//     override def update(f: Metadata => Metadata): Primitive.Required[A]

//   object Required:
//     final case class Root[A](metadata: Metadata, tpe: Type[A]) extends Primitive.Required[A]:
//       override def constraints: Chain[Constraint.Primitive[?]] = Chain.empty
//       override def update(f: Metadata => Metadata): Primitive.Required[A] = copy(metadata = f(metadata))

//     final case class Transform[A, B](
//         self: Primitive.Required[A],
//         validation: SchemaValidation.Primitive[A, B],
//         f: B => A
//     ) extends Primitive.Required[B]:
//       export self.{metadata, tpe}
//       override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
//       override def update(f: Metadata => Metadata): Primitive.Required[B] = copy(self = self.update(f))

//   final case class Optional[A](self: Primitive[A]) extends Primitive[Option[A]]:
//     export self.{constraints, metadata, tpe}
//     override def update(f: Metadata => Metadata): Primitive[Option[A]] = copy(self = self.update(f))

//   final case class Transform[A, B](
//       self: Primitive[A],
//       validation: SchemaValidation.Primitive[A, B],
//       f: B => A
//   ) extends Primitive[B]:
//     export self.{metadata, tpe}
//     override def constraints: Chain[Constraint.Primitive[?]] = self.constraints ++ validation.constraints
//     override def update(f: Metadata => Metadata): Primitive[B] = copy(self = self.update(f))

// sealed trait Product[+B, C] extends Schema[B, C]:
//   override def imap[D](f: C => D)(g: D => C): Product[B, D] = Product.Transform(this, f, g)
//   override def optional: Product[B, Option[C]] = Product.Optional(this)
//   def productWith[D, E](merge: (Metadata, Metadata) => Metadata)(product: Product[D, E]): Product[B & D, (C, E)] =
//     Product.Combine(merge(metadata, product.metadata), this, product)
//   def schemas: Chain[Schema[?, ?]]
//   override def update(f: Metadata => Metadata): Product[B, C]

// object Product:
//   final case class Combine[B, C, D, E](metadata: Metadata, left: Product[B, C], right: Product[D, E])
//       extends Product[B & D, (C, E)]:
//     override def schemas: Chain[Schema[?, ?]] = left.schemas ++ right.schemas
//     override def update(f: Metadata => Metadata): Product[B & D, (C, E)] = copy(metadata = f(metadata))

//   case class Empty(metadata: Metadata) extends Product[Nothing, Unit]:
//     override def schemas: Chain[Nothing] = Chain.empty
//     override def update(f: Metadata => Metadata): Product[Nothing, Unit] = copy(metadata = f(metadata))

//   final case class One[O <: Schema[?, A], A](metadata: Metadata, schema: O) extends Product[O, A]:
//     override def schemas: Chain[Schema[?, ?]] = Chain.one(schema)
//     override def update(f: Metadata => Metadata): Product[O, A] = copy(metadata = f(metadata))

//   final case class Optional[B, C](self: Product[B, C]) extends Product[B, Option[C]]:
//     export self.{metadata, schemas}
//     override def update(f: Metadata => Metadata): Product[B, Option[C]] = copy(self = self.update(f))

//   final case class Transform[B, C, D](self: Product[B, C], f: C => D, g: D => C) extends Product[B, D]:
//     export self.{metadata, schemas}
//     override def update(f: Metadata => Metadata): Product[B, D] = copy(self = self.update(f))

// sealed trait Record[+B, C] extends Schema[B, C]:
//   def fields: Chain[Field[?, ?]]
//   override def imap[D](f: C => D)(g: D => C): Record[B, D] = Record.Transform(this, f, g)
//   override def optional: Record[B, Option[C]] = Record.Optional(this)
//   def productWith[D, E](merge: (Metadata, Metadata) => Metadata)(product: Record[D, E]): Record[B & D, (C, E)] =
//     Record.Combine(merge(metadata, product.metadata), this, product)
//   override def update(f: Metadata => Metadata): Record[B, C]

// object Record:
//   final case class Empty(metadata: Metadata) extends Record[Nothing, Unit]:
//     override def fields: Chain[Nothing] = Chain.empty
//     override def update(f: Metadata => Metadata): Record[Nothing, Unit] = copy(metadata = f(metadata))

//   final case class Combine[B, C, D, E](metadata: Metadata, left: Record[B, C], right: Record[D, E])
//       extends Record[B & D, (C, E)]:
//     override def fields: Chain[Field[?, ?]] = left.fields ++ right.fields
//     override def update(f: Metadata => Metadata): Record[B & D, (C, E)] = copy(metadata = f(metadata))

//   final case class One[B, C](metadata: Metadata, field: Field[B, C]) extends Record[B, C]:
//     override def fields: Chain[Field[?, ?]] = Chain.one(field)
//     override def update(f: Metadata => Metadata): Record[B, C] = copy(metadata = f(metadata))

//   final case class Optional[B, C](self: Record[B, C]) extends Record[B, Option[C]]:
//     export self.{fields, metadata}
//     override def update(f: Metadata => Metadata): Record[B, Option[C]] = copy(self = self.update(f))

//   final case class Transform[B, C, D](self: Record[B, C], f: C => D, g: D => C) extends Record[B, D]:
//     export self.{fields, metadata}
//     override def update(f: Metadata => Metadata): Record[B, D] = copy(self = self.update(f))

// sealed trait Sum[+B, C] extends Schema[B, C]:
//   def branches: NonEmptyChain[Branch[?, ?]]
//   override def imap[D](f: C => D)(g: D => C): Sum[B, D] = Sum.Transform(this, f, g)
//   override def optional: Sum[B, Option[C]] = Sum.Optional(this)
//   def orElseWith[D, E](merge: (Metadata, Metadata) => Metadata)(sum: Sum[D, E]): Sum[B | D, Either[C, E]] =
//     Sum.Combine(merge(metadata, sum.metadata), this, sum)
//   override def update(f: Metadata => Metadata): Sum[B, C]

// object Sum:
//   final case class Combine[B, C, D, E](metadata: Metadata, left: Sum[B, C], right: Sum[D, E])
//       extends Sum[B | D, Either[C, E]]:
//     override def branches: NonEmptyChain[Branch[?, ?]] = left.branches ++ right.branches
//     override def update(f: Metadata => Metadata): Sum[B | D, Either[C, E]] = copy(metadata = f(metadata))

//   final case class Optional[B, C](self: Sum[B, C]) extends Sum[B, Option[C]]:
//     export self.{branches, metadata}
//     override def update(f: Metadata => Metadata): Sum[B, Option[C]] = copy(self = self.update(f))

//   final case class Root[B, C](metadata: Metadata, branch: Branch[B, C]) extends Sum[B, C]:
//     override def branches: NonEmptyChain[Branch[B, C]] = NonEmptyChain.one(branch)
//     override def update(f: Metadata => Metadata): Sum[B, C] = copy(metadata = f(metadata))

//   final case class Transform[B, C, D](self: Sum[B, C], f: C => D, g: D => C) extends Sum[B, D]:
//     export self.{branches, metadata}
//     override def update(f: Metadata => Metadata): Sum[B, D] = copy(self = self.update(f))

// sealed trait Union[+B, C] extends Schema[B, C]:
//   override def imap[D](f: C => D)(g: D => C): Union[B, D] = Union.Transform(this, f, g)
//   override def optional: Union[B, Option[C]] = Union.Optional(this)
//   def orElseWith[D, E](merge: (Metadata, Metadata) => Metadata)(union: Union[D, E]): Union[B | D, Either[C, E]] =
//     Union.Combine(merge(metadata, union.metadata), this, union)
//   override def update(f: Metadata => Metadata): Union[B, C]

// object Union:
//   sealed trait Value[+B, C] extends Base.Value[B, C], Union[B, C]:
//     override def imap[D](f: C => D)(g: D => C): Union.Value[B, D] = Value.Transform(this, f, g)
//     final override def optional: Union.Value[B, Option[C]] = Value.Optional(this)
//     def orElseWith[D, E](merge: (Metadata, Metadata) => Metadata)(
//         union: Union.Value[D, E]
//     ): Union.Value[B | D, Either[C, E]] = Value.Combine(merge(metadata, union.metadata), this, union)
//     override def update(f: Metadata => Metadata): Union.Value[B, C]

//   object Value:
//     sealed trait Required[+B, C] extends Base.Value.Required[B, C], Union.Value[B, C]:
//       override def imap[D](f: C => D)(g: D => C): Union.Value.Required[B, D] = Required.Transform(this, f, g)
//       def orElseWith[D, E](merge: (Metadata, Metadata) => Metadata)(
//           union: Union.Value.Required[D, E]
//       ): Union.Value.Required[B | D, Either[C, E]] = Required.Combine(merge(metadata, union.metadata), this, union)
//       override def update(f: Metadata => Metadata): Union.Value.Required[B, C]

//     object Required:
//       final case class Combine[B, C, D, E](
//           metadata: Metadata,
//           left: Union.Value.Required[B, C],
//           right: Union.Value.Required[D, E]
//       ) extends Union.Value.Required[B | D, Either[C, E]]:
//         override def update(f: Metadata => Metadata): Union.Value.Required[B | D, Either[C, E]] =
//           copy(metadata = f(metadata))

//       final case class Transform[B, C, D](self: Union.Value.Required[B, C], f: C => D, g: D => C)
//           extends Union.Value.Required[B, D]:
//         export self.metadata
//         override def update(f: Metadata => Metadata): Union.Value.Required[B, D] = copy(self = self.update(f))

//     final case class Combine[B, C, D, E](metadata: Metadata, left: Union.Value[B, C], right: Union.Value[D, E])
//         extends Union.Value[B | D, Either[C, E]]:
//       override def update(f: Metadata => Metadata): Union.Value[B | D, Either[C, E]] = copy(metadata = f(metadata))

//     final case class Optional[B, C](self: Union.Value[B, C]) extends Union.Value[B, Option[C]]:
//       export self.metadata
//       override def update(f: Metadata => Metadata): Union.Value[B, Option[C]] = copy(self = self.update(f))

//     final case class Transform[B, C, D](self: Union.Value[B, C], f: C => D, g: D => C) extends Union.Value[B, D]:
//       export self.metadata
//       override def update(f: Metadata => Metadata): Union.Value[B, D] = copy(self = self.update(f))

//   final case class Combine[B, C, D, E](metadata: Metadata, left: Union[B, C], right: Union[D, E])
//       extends Union[B | D, Either[C, E]]:
//     override def update(f: Metadata => Metadata): Union[B | D, Either[C, E]] = copy(metadata = f(metadata))

//   final case class Optional[B, C](self: Union[B, C]) extends Union[B, Option[C]]:
//     export self.metadata
//     override def update(f: Metadata => Metadata): Union[B, Option[C]] = copy(self = self.update(f))

//   final case class Root[B <: Schema[?, C], C](metadata: Metadata, schema: B) extends Union[B, C]:
//     override def update(f: Metadata => Metadata): Union[B, C] = copy(metadata = f(metadata))

//   final case class Transform[B, C, D](self: Union[B, C], f: C => D, g: D => C) extends Union[B, D]:
//     export self.metadata
//     override def update(f: Metadata => Metadata): Union[B, D] = copy(self = self.update(f))
