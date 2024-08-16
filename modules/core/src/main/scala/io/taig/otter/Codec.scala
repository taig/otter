package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

abstract class Codec[+F[+a] <: Data.Optional[a], +O <: Data, A]:
  def isOptional: Boolean

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[F, O, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, O, B]
  def to[B](using Convert[A, B]): Codec[F, O, B]

  def optional: Codec[Data.Optional, O, Option[A]]

  def decode(data: Data): Codec.Result[A]
  def encode(a: A): F[O]

object Codec:
  type Result[A] = Validated[Violations, A]

  extension [A](self: Codec[Data.Optional, Data.Primitive, A])
    def parseOptional(value: Option[String]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(Data.String.apply))
    def printOptional(a: A): Option[String] = self.encode(a) match
      case Data.Null            => none
      case data: Data.Primitive => data.plain.some

  extension [A](self: Codec[Data.Required, Data.Primitive, A])
    def parseRequired(value: String): Codec.Result[A] = self.decode(Data.String(value))
    def printRequired(a: A): String = self.encode(a).plain

  extension [A](self: Codec[Data.Optional, Data.Array[Data.Primitive], A])
    def parseOptionalArray(value: Option[Vector[String]]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(values => Data.Array(values.map(Data.String.apply))))
    def printOptionalArray(a: A): Option[Vector[String]] = self.encode(a) match
      case Data.Null          => none
      case Data.Array(values) => values.map(_.plain).some

  extension [A](self: Codec[Data.Required, Data.Array[Data.Primitive], A])
    def parseArray(values: Vector[String]): Codec.Result[A] =
      self.decode(Data.Array(values.map(Data.String.apply)))
    def printArray(a: A): Vector[String] = self.encode(a).values.map(_.plain)

  extension [A](self: Codec[Data.Required, Data.Object[Data.Optional[Data.Primitive]], A])
    def parseObject(value: Vector[(String, String)]): Codec.Result[A] = ???
    def printObject(a: A): Vector[(String, String)] = ???

  extension [A](self: Codec[Data.Optional, Data.Object[Data.Optional[Data.Primitive]], A])
    def parseOptionalObject(value: Option[Vector[(String, String)]]): Codec.Result[A] =
      self.decode(value.fold(Data.Null): values =>
        Data.Object(values.map { case (key, value) => (key, Data.String(value)) }))
    def printOptionalObject(a: A): Option[Vector[(String, String)]] =
      (self.encode(a): Data.Optional[Data.Object[Data.Optional[Data.Primitive]]]) match
        case Data.Null => none
        case Data.Object(values) =>
          values
            .mapFilter:
              case (key, data: Data.Primitive) => (key, data.plain).some
              case (_, Data.Null)              => none
            .some

  given [F[+a] <: Data.Optional[a], O <: Data]: CodecInvariant[Codec[F, O, *]] =
    new CodecInvariant[Codec[F, O, *]]:
      override def imap[A, B](fa: Codec[F, O, A])(f: A => B)(g: B => A): Codec[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Optional[a], O <: Data, A]: Metadata.Ops[Codec[F, O, A]] with
    extension (self: Codec[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A] = self.modifyMetadata(f)
