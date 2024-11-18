package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

abstract class Codec[+O <: Data, A]:
  self =>

  final def isNullable: Boolean = this match
    case _: Nullable[?, ?] => true
    case _                 => false

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[O, A]

  def imap[B](f: A => B)(g: B => A): Codec[O, B]
  def to[B](using Convert[A, B]): Codec[O, B]

  final def :*[P <: Data, B](codec: Codec[P, B]): Tuple[O | P, (A, B)] = toTuple.zip(codec.toTuple)
  final def *:[P <: Data, B](codec: Codec[P, B]): Tuple[P | O, (B, A)] = codec.toTuple.zip(toTuple)

  def decode(data: Data): Codec.Result[A]
  def encode(a: A): O

  final def toTuple: Tuple[O, A] = Tuple(this)

object Codec:
  type Result[A] = Validated[Violations, A]

  extension [O <: Data.Value, A](self: Codec[O, A])
    def nullable(default: A): Nullable[O, A] = Nullable(self, default)
    def nullable: Nullable[O, Option[A]] = Nullable(self)

  extension [A](self: Codec[Data.Primitive, A])
    def parse(value: String): Codec.Result[A] = self.decode(Data.String(value))
    def print(a: A): String = self.encode(a).plain

  extension [A](self: Codec[Data.Nullable[Data.Primitive], A])
    def parseNullable(value: Option[String]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(Data.String.apply))
    def printNullable(a: A): Option[String] = self.encode(a) match
      case Data.Null            => none
      case data: Data.Primitive => data.plain.some

  extension [A](self: Codec[Data.Array[Data.Primitive], A])
    def parseArray(values: Vector[String]): Codec.Result[A] =
      self.decode(Data.Array(values.map(Data.String.apply)))
    def printArray(a: A): Vector[String] = self.encode(a).values.map(_.plain)

  extension [A](self: Codec[Data.Nullable[Data.Array[Data.Primitive]], A])
    def parseNullableArray(value: Option[Vector[String]]): Codec.Result[A] =
      self.decode(value.fold(Data.Null)(values => Data.Array(values.map(Data.String.apply))))
    def printNullableArray(a: A): Option[Vector[String]] = self.encode(a) match
      case Data.Null                        => none
      case data: Data.Array[Data.Primitive] => data.values.map(_.plain).some

  extension [A](self: Codec[Data.Object[Data.Nullable[Data.Primitive]], A])
    def parseObject(values: Vector[(String, Option[String])]): Codec.Result[A] =
      val data = Data.Object(values.map {
        case (name, Some(value)) => (name, Data.String(value))
        case (name, None)        => (name, Data.Null)
      })
      self.decode(data)
    def printObject(a: A): Vector[(String, Option[String])] =
      self
        .encode(a)
        .values
        .map:
          case (name, data: Data.Primitive) => (name, Some(data.plain))
          case (name, Data.Null)            => (name, none)

  given [O <: Data]: CodecInvariant[Codec[O, *]] =
    new CodecInvariant[Codec[O, *]]:
      override def imap[A, B](fa: Codec[O, A])(f: A => B)(g: B => A): Codec[O, B] = fa.imap(f)(g)

  given [O <: Data, A]: Metadata.Ops[Codec[O, A]] with
    extension (self: Codec[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Codec[O, A] = self.modifyMetadata(f)
