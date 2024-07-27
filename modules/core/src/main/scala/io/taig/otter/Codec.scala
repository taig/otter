package io.taig.otter

import io.taig.otter
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Codec[+F[+a <: Data] <: Data.Optional[a], +O <: Data.Value, A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[F, O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[F, O, A]

  def imap[B](f: A => B)(g: B => A): Codec[F, O, B]

  def optional: Codec[Data.Optional, O, Option[A]]

  def decode(data: Data): Codec.Result[A]
  def encode(a: A): F[O]

  // final def toCollection: Collection[Data.Array[O], Vector[A]] = Collection(this)
  // final def toProduct: Product[Data.Array[O], A] = Product(this)
  // final def toUnion: Union[O, A] = Union(this)

object Codec:
  type Result[A] = Validated[Violations[Violation[Constraint.Any[Data], Data]], A]
