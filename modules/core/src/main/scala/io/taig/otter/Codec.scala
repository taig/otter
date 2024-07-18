package io.taig.otter

import io.taig.otter
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant

abstract class Codec[+O, A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[O, A]

  def default: Option[A]
  def modifyDefault(f: Option[A] => Option[A]): Codec[O, A]

  def imap[B](f: A => B)(g: B => A): Codec[O, B]

  final def collection: Collection[this.type, Vector[A]] = Collection(this)

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

  object Required:
    given [O]: Invariant[Codec.Required[O, *]] with
      override def imap[A, B](fa: Codec.Required[O, A])(f: A => B)(g: B => A): Codec.Required[O, B] = fa.imap(f)(g)

  given [O]: Invariant[Codec[O, *]] with
    override def imap[A, B](fa: Codec[O, A])(f: A => B)(g: B => A): Codec[O, B] = fa.imap(f)(g)
