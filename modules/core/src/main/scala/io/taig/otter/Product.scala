package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.data.Validated
import io.taig.otter.Data.Optional
import io.taig.otter.Codec.Result
import cats.Id
import cats.Traverse
import cats.Applicative

sealed abstract class Product[
    +F[+a <: Data] <: Data.Optional[a],
    +G[+a <: Data] <: Data.Object[a] | Data.Array[a],
    +O <: Data,
    A
] extends Codec[F, G[O], A]:
  self =>

  def fields: Fields[?, ?]
  override def modifyMetadata(f: Metadata => Metadata): Product[F, G, O, A]
  override def modifyDefault(f: Option[A] => Option[A]): Product[F, G, O, A]
  override def imap[B](f: A => B)(g: B => A): Product[F, G, O, B]
  override def optional: Product[Data.Optional, G, O, Option[A]]

sealed abstract class Record[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Object, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Record[F, O, A] = ???

  final override def modifyDefault(f: Option[A] => Option[A]): Record[F, O, A] = ???

  final override def imap[B](f: A => B)(g: B => A): Record[F, O, B] = ???

  override def optional: Record[Data.Optional, O, Option[A]] = new Record[Data.Optional, O, Option[A]]:
    export self.{fields, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], Option[A])] =
      data.fold(Validated.valid(default.flatten).tupleLeft(data))(_ => self.decode(data).map(_.map(_.some)))
    override def encode(a: Option[A]): Data.Optional[Data.Object[O]] = a.map(self.encode).getOrElse(Data.Null)

  def product[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Record[G, P, B]
  ): Record[Id, F[O] | G[P], (A, B)] = new Record[Id, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.product(codec.fields)
    override def default: Option[(A, B)] = None
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Option[Chain[(String, Data)]]): Result[(Option[Chain[(String, Data)]], (A, B))] = ???
    override def encode(ab: (A, B)): Data.Object[F[O] | G[P]] =
      self.encode(ab._1).fill ++ codec.encode(ab._2).fill

  final override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Object(values) => decode(values.some)
      case Data.Null           => decode(none)
      case _ => Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)]

// object Record:
//   extension [O <: Data, A](self: Record[Data.Optional[Data.Object[O]], A])
//     def product[P <: Data, B](codec: Record[Data.Optional[Data.Object[P]], B]): Record[Data.Object[O | P], (A, B)] =
//       new Record[Data.Object[O | P], (A, B)]:
//         override def fields: Fields[?, ?] = self.fields.product(codec.fields)
//         override def metadata: Metadata = Metadata.Empty
//         override def default: Option[(A, B)] = None
//         override def decode(data: Option[Chain[(String, Data)]]): Result[(Option[Chain[(String, Data)]], (A, B))] = ???
//         override def encode(ab: (A, B)): Data.Object[O | P] = (self.encode(ab._1) match {
//           case Data.Null => Data.Object.Empty
//           case data: Data.Object[O] => data
//         }) ++ (codec.encode(ab._2) match {
//           case Data.Null => Data.Object.Empty
//           case data: Data.Object[P] => data
//         })

// // object Record:
// //   def apply[O <: Data, A](fields: Fields[O, A]): Record[Data.Optional.Some, O, A] =
// //     val _fields = fields

// //     new Record[Data.Optional.Some, O, A]:
// //       override def fields: Fields[O, A] = _fields
// //       override def metadata: Metadata = Metadata.Empty
// //       override def default: Option[A] = None
// //       override def decode(data: Option[Chain[(String, Data)]]): Codec.Result[(Option[Chain[(String, Data)]], A)] =
// //         data
// //           .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String("null"))))
// //           .andThen(fields.decodeRecord(_).map(_.leftMap(_.some)))
// //       override def encode(a: A): Data.Optional.Some[Data.Object[O]] =
// //         Data.Optional.Some(Data.Object(fields.encodeRecord(a)))

sealed abstract class Tuple[+F[+a <: Data] <: Data.Optional[a]: Data.Ops, +O <: Data, A]
    extends Product[F, Data.Array, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Tuple[F, O, A] = ???

  final override def modifyDefault(f: Option[A] => Option[A]): Tuple[F, O, A] = ???

  final override def imap[B](f: A => B)(g: B => A): Tuple[F, O, B] = ???

  override def optional: Tuple[Data.Optional, O, Option[A]] = ???

  def product[G[+a <: Data] <: Data.Optional[a]: Data.Ops, P <: Data, B](
      codec: Tuple[G, P, B]
  ): Tuple[Id, F[O] | G[P], (A, B)] = new Tuple[Id, F[O] | G[P], (A, B)]:
    override def fields: Fields[?, ?] = self.fields.product(codec.fields)
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[(A, B)] = None
    override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], (A, B))] = ???
    override def encode(ab: (A, B)): Data.Array[F[O] | G[P]] =
      self.encode(ab._1).fill(self.fields.toChain.length.toInt) ++
        codec.encode(ab._2).fill(codec.fields.toChain.length.toInt)

  override def decode(data: Data): Codec.Result[A] = data
    .match
      case Data.Null          => decode(none)
      case Data.Array(values) => decode(values.some)
      case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid
    .map { case (_, a) => a }

  def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)]

// object Tuple:
//   extension [O <: Data, A](self: Tuple[Data.Optional[Data.Array[O]], A])
//     def product[P <: Data, B](codec: Tuple[Data.Optional[Data.Array[P]], B]): Tuple[Data.Array[O | P], (A, B)] =
//       new Tuple[Data.Array[O | P], (A, B)]:
//         override def fields: Fields[?, ?] = self.fields.product(codec.fields)
//         override def default: Option[(A, B)] = None
//         override def metadata: Metadata = Metadata.Empty
//         override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], (A, B))] = ???
//         override def encode(ab: (A, B)): Data.Array[O | P] =
//           self.encode(ab._1)
//           ???

// // //   def apply[O <: Data, A](fields: Fields[O, A]): Tuple[Data.Array[O], A] =
// // //     val _fields = fields

// // //     new Tuple[Data.Array[O], A]:
// // //       override def fields: Fields[O, A] = _fields
// // //       override def metadata: Metadata = Metadata.Empty
// // //       override def default: Option[A] = None
// // //       override def decode(data: Option[Vector[Data]]): Codec.Result[(Option[Vector[Data]], A)] = data
// // //         .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
// // //         .andThen(fields.decodeArray(_).map(_.leftMap(_.some)))
// // //       override def encode(a: A): Data.Array[O] = Data.Array(fields.encodeArray(a))
