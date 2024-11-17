package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Constant[+O <: Data.Primitive, A] extends Codec[O, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Constant[O, A] = new Constant[O, A]:
    export self.{decode, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Constant[O, B] = new Constant[O, B]:
    export self.metadata
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  override def to[B](using convert: Convert[A, B]): Constant[O, B] = imap(convert.to)(convert.from)

object Constant:
  def apply[O <: Data.Primitive, A](codec: Codec[O, A], value: A): Constant[O, Unit] = new Constant[O, Unit]:
    val constant = codec.encode(value)
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[Unit] =
      Validated.cond(data === constant, (), Violations.rootNec(Violation.tpe(constant.plain, data)))
    override def encode(a: Unit): O = constant
