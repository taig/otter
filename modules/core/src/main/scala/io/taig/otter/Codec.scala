package io.taig.otter

import io.taig.otter
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Codec[+O, A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[O, A]

  def imap[B](f: A => B)(g: B => A): Codec[O, B]

  def optional: Codec[O, Option[A]]

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): Data

object Codec:
  type Result[A] = Validated[Violations[Violation[Constraint.Any[Data], Data]], A]

  trait Required[+O, A] extends Codec[O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Codec.Required[O, A]

    override def imap[B](f: A => B)(g: B => A): Codec.Required[O, B]

    final override def decode(data: Data): Codec.Result[A] = data match
      case data: Data.Value => decodeValue(data)
      case Data.Null =>
        Violations.rootNec(Violation(Constraint.Type(data.name), actual = Data.String("null"))).invalid

    def decodeValue(data: Data.Value): Codec.Result[A]

    final override def encode(a: A): Data = encodeValue(a)

    def encodeValue(a: A): Data.Value

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
