package io.taig.otter

import io.taig.otter
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant

abstract class Codec[+O <: Data, A]:
  self =>

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[O, A]

  def imap[B](f: A => B)(g: B => A): Codec[O, B]

  def optional: Codec[Data.Optional[O], Option[A]]

  // final def toCollection: Collection[this.type, Vector[A]] = ??? // Collection(this)
  // final def toProduct: Product[this.type, A] = Product(this)
  // def toUnion: Union[this.type, A] = Union(this)

  def decode(data: Data): Codec.Result[A]

  def encode(a: A): O

object Codec:
  type Result[A] = Validated[Violations[Violation[Constraint.Any[Data], Data]], A]

  extension [O <: Data.Optional[Data.Primitive], A](self: Codec[O, A])
    def parseOptional(value: Option[String]): Codec.Result[A] = ???
    def printOptional(a: A): Option[String] = ???

  extension [O <: Data.Primitive, A](self: Codec[O, A])
    def parseRequired(value: String): Codec.Result[A] = ???
    def printRequired(a: A): String = ???

  // extension [O, A](self: Codec[O, A])
  //   def :*[B](codec: Codec[?, B])(using merge: Evidence.Merge[A, B]): Product[self.type | codec.type, merge.Out] =
  //     self.toProduct :* codec
  //   def *:[B](codec: Codec[?, B])(using merge: Evidence.Merge[A, B]): Product[self.type | codec.type, merge.Out] =
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

  given [O <: Data]: Invariant[Codec[O, *]] with
    override def imap[A, B](fa: Codec[O, A])(f: A => B)(g: B => A): Codec[O, B] = fa.imap(f)(g)
