package io.taig.otter

import io.taig.otter
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Codec[+F[+a] <: Data.Optional[a], +O <: Data, A]:
  self =>

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[F, O, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, O, B]

  def optional: Codec[Data.Optional, O, Option[A]]

  // final def toCollection: Collection[Data.Array[O], Vector[A]] = Collection(this)
  // final def toProduct: Product[Data.Array[O], A] = Product(this)
  // final def toUnion: Union[O, A] = Union(this)

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): O

object Codec:
  type Result[A] = Validated[Violations[Violation[Constraint.Any[Data], Data]], A]

  // extension [A](self: Codec[Data.Optional, Data.Primitive, A])
  //   def parseOptional(value: Option[String]): Codec.Result[A] = ???
  //   def printOptional(a: A): Option[String] = ???

  // extension [A](self: Codec[Data.Required, Data.Primitive, A])
  //   def parseRequired(value: String): Codec.Result[A] = ???
  //   def printRequired(a: A): String = ???

  // extension [O <: Data, A](self: Codec[O, A])
  //   def :*[P <: Data, B](codec: Codec[P, B])(using
  //       merge: Evidence.Merge[A, B]
  //   ): Product[Data.Array[O | P], merge.Out] =
  //     self.toProduct :* codec
  //   def *:[P <: Data, B](codec: Codec[P, B])(using
  //       merge: Evidence.Merge[A, B]
  //   ): Product[Data.Array[O | P], merge.Out] =
  //     self.toProduct :* codec

  // extension [O, A](self: Codec[O, A])
  //   def :+[B](codec: Codec[?, B]): Union[self.type & codec.type, Either[A, B]] = self.toUnion :+ codec
  //   def +:[B](codec: Codec[?, B]): Union[self.type & codec.type, Either[A, B]] = self.toUnion :+ codec

  // extension [O, A <: Matchable](self: Codec[O, A])
  //   inline def |[B <: Matchable](codec: Codec[?, B]): Union[self.type & codec.type, A | B] =
  //     (self :+ codec).imap {
  //       case Left(a)  => a
  //       case Right(b) => b
  //     } {
  //       case a: A => Left(a)
  //       case b: B => Right(b)
  //     }

  // given [O <: Data]: Invariant[Codec[O, *]] with
  //   override def imap[A, B](fa: Codec[O, A])(f: A => B)(g: B => A): Codec[O, B] = fa.imap(f)(g)
