package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.Eval

sealed abstract class Constant[+O <: Data.Primitive, A] extends Codec[O, A]:
  self =>

  def codec: Eval[Codec[?, ?]]

  override def modifyMetadata(f: Metadata => Metadata): Constant[O, A] = new Constant[O, A]:
    export self.{decode, encode, codec}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Constant[O, B] = new Constant[O, B]:
    export self.{codec, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  override def to[B](using convert: Convert[A, B]): Constant[O, B] = imap(convert.to)(convert.from)

object Constant:
  final private [otter] case class Apply[O <: Data.Primitive, A](codec: Eval[Codec[O, A]], value: Eval[A]) extends Constant[O, Unit]:
    val constant = (codec, value).mapN(_.encode(_))
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[Unit] =
      Validated.cond(data === constant.value, (), Violations.rootNec(Violation.tpe(constant.value.plain, data)))
    override def encode(a: Unit): O = constant.value
