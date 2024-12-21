package io.taig.otter

import cats.Eval
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Constant[+O <: Data.Primitive, A] extends Codec[O, A]:
  self =>

  def codec: Eval[Codec[?, ?]]

  def data: O

  override def modifyMetadata(f: Metadata => Metadata): Constant[O, A] = new Constant[O, A]:
    export self.{codec, data, decode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Constant[O, B] = new Constant[O, B]:
    export self.{codec, data, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)

  override def to[B](using convert: Convert[A, B]): Constant[O, B] = imap(convert.to)(convert.from)
  final override def encode(a: A): O = data

object Constant:
  final private case class Apply[O <: Data.Primitive, A](codec: Eval[Codec[O, A]], value: Eval[A])
      extends Constant[O, Unit]:
    val constant = (codec, value).mapN(_.encode(_))
    override def data: O = constant.value
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[Unit] =
      Validated.cond(data === constant.value, (), Violations.rootNec(Violation.tpe(constant.value.plain, data)))

  inline def apply[O <: Data.Primitive, A](codec: Eval[Codec[O, A]], value: Eval[A]): Constant[O, Unit] =
    Apply(codec, value)
