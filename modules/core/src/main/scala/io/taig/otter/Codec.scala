package io.taig.otter

import io.taig.otter
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Codec[+F[+a], +O <: Data, A] extends Codec.Reader[F, O, A], Codec.Writer[F, O, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A]

  override def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Codec[F, O, A1]

  def imap[B](f: A => B)(g: B => A): Codec[F, O, B]

  override def optional: Codec[Data.Optional, O, Option[A]]

  // final def toCollection: Collection[Data.Array[O], Vector[A]] = Collection(this)
  // final def toProduct: Product[Data.Array[O], A] = Product(this)
  // final def toUnion: Union[O, A] = Union(this)

object Codec:
  type Result[A] = Validated[Violations[Violation[Constraint.Any[Data], Data]], A]

  trait Reader[+F[+_], +O <: Data, +A]:
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Codec.Reader[F, O, A]
    def default: Option[A]
    def modifyDefault[A1 >: A](f: Option[A1] => Option[A1]): Codec.Reader[F, O, A1]
    def map[B](f: A => B): Codec.Reader[F, O, B]
    def optional: Codec.Reader[Data.Optional, O, Option[A]]
    def decode(data: Data): Codec.Result[A]

  trait Writer[+F[+_], +O <: Data, -A]:
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Codec.Writer[F, O, A]
    def contramap[B](f: B => A): Codec.Writer[F, O, B]
    def optional: Codec.Writer[Data.Optional, O, Option[A]]
    def encode(a: A): F[O]
