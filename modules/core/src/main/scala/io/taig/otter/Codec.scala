package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Codec[+F[+a] <: Data.Optional[a], A]:
  self =>

  type Of

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec.Of[F, Of, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec.Of[F, Of, A]

  def imap[B](f: A => B)(g: B => A): Codec.Of[F, Of, B]

  def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Codec.Of[F, Of, B]

  // TODO move into typeclass
  def const(a: A): Codec.Of[F, Of, Unit] = imap(_ => ())(_ => a)

  def optional: Codec.Of[Data.Optional, Of, Option[A]]

  def decode(data: Data): Codec.Result[A]
  def encode(a: A): F[Of]

  // final def toCollection: Collection.Of[Identity, self.Out, Vector[A]] = Collection(this)

object Codec:
  type Of[+F[+a] <: Data.Optional[a], O, A] = Codec[F, A] { type Of = O }

  // extension [A](self: Codec.Of[Data.Optional, Data.Primitive, A])
  //   def parseOptional(value: Option[String]): Codec.Result[A] = ???
  //   def printOptional(a: A): Option[String] = (self.encode(a): Data.Optional[Data.Primitive]) match
  //     case Data.Null            => none
  //     case data: Data.Primitive => data.print.some

  // extension [A](self: Codec.Of[Identity, Data.Primitive, A])
  //   def parseRequired(value: String): Codec.Result[A] = ???
  //   def printRequired(a: A): String = ???

  // extension [A](self: Codec.Of[Data.Optional, Data.Array[Data.Primitive], A])
  //   def parseOptionalArray(value: Option[Vector[String]]): Codec.Result[A] = ???
  //   def printOptionalArray(a: A): Option[Vector[String]] =
  //     (self.encode(a): Data.Optional[Data.Array[Data.Primitive]]) match
  //       case Data.Null          => none
  //       case Data.Array(values) => ??? // values.map(_.print).some

  // extension [A](self: Codec.Of[Identity, Data.Array[Data.Primitive], A])
  //   def parseArray(value: Vector[String]): Codec.Result[A] = ???
  //   def printArray(a: A): Vector[String] = self.encode(a).values.map(_.print)

  // extension [A](self: Codec.Of[Data.Optional, Data.Object[Data.Optional[Data.Primitive]], A])
  //   def parseOptionalObject(value: Option[Vector[(String, String)]]): Codec.Result[A] = ???
  //   def printOptionalObject(a: A): Option[Vector[(String, String)]] =
  //     (self.encode(a): Data.Optional[Data.Object[Data.Optional[Data.Primitive]]]) match
  //       case Data.Null => none
  //       case Data.Object(values) =>
  //         values.mapFilter {
  //           case (key, data: Data.Primitive) => (key, data.print).some
  //           case (_, Data.Null)              => none
  //         }.some

  type Result[A] = Validated[Violations[Violation[Constraint[Data], Data]], A]
