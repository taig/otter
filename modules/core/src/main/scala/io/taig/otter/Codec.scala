package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

abstract class Codec[+F[+a] <: Data.Optional[a], +O <: Data, A]:
  self =>

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[F, O, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, O, B]

  // TODO move into typeclass
  def const(a: A): Codec[F, O, Unit] = imap(_ => ())(_ => a)

  def optional: Codec[Data.Optional, O, Option[A]]

  def decode(data: Data): Codec.Result[A]
  def encode(a: A): F[O]

object Codec:
  extension [A](self: Codec[Data.Optional, Data.Primitive, A])
    def parseOptional(value: Option[String]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(Data.String.apply))
    def printOptional(a: A): Option[String] = self.encode(a) match
      case Data.Null            => none
      case data: Data.Primitive => data.print(quoted = false).some

  extension [A](self: Codec[Data.Required, Data.Primitive, A])
    def parseRequired(value: String): Codec.Result[A] = self.decode(Data.String(value))
    def printRequired(a: A): String = self.encode(a).print(quoted = false)

  extension [A](self: Codec[Data.Optional, Data.Array[Data.Primitive], A])
    def parseOptionalArray(value: Option[Vector[String]]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(values => Data.Array(values.map(Data.String.apply))))
    def printOptionalArray(a: A): Option[Vector[String]] = self.encode(a) match
      case Data.Null          => none
      case Data.Array(values) => values.map(_.print(quoted = false)).some

  extension [A](self: Codec[Data.Required, Data.Array[Data.Primitive], A])
    def parseArray(value: Vector[String]): Codec.Result[A] = ???
    def printArray(a: A): Vector[String] = self.encode(a).values.map(_.print(quoted = false))

  extension [A](self: Codec[Data.Optional, Data.Object[Data.Optional[Data.Primitive]], A])
    def parseOptionalObject(value: Option[Vector[(String, String)]]): Codec.Result[A] = ???
    def printOptionalObject(a: A): Option[Vector[(String, String)]] =
      (self.encode(a): Data.Optional[Data.Object[Data.Optional[Data.Primitive]]]) match
        case Data.Null => none
        case Data.Object(values) =>
          values.mapFilter {
            case (key, data: Data.Primitive) => (key, data.print(quoted = false)).some
            case (_, Data.Null)              => none
          }.some

  type Result[A] = Validated[Violations, A]
