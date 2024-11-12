package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec.Result

sealed abstract class Constant[A] extends Codec[Data.Primitive, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Constant[A] = new Constant[A]:
    export self.{decode, encode}
    override def metadata: Metadata = f(self.metadata)

  override def imap[B](f: A => B)(g: B => A): Constant[B] = new Constant[B]:
    export self.metadata
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Primitive = self.encode(g(b))

  override def to[B](using convert: Convert[A, B]): Constant[B] = imap(convert.to)(convert.from)

object Constant:
  def apply[A](codec: Codec[Data.Primitive, A], value: A): Constant[Unit] = new Constant[Unit]:
    val constant = codec.encode(value)
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[Unit] =
      Validated.cond(data === constant, (), Violations.rootNec(Violation.tpe(constant.plain, data)))

    override def encode(a: Unit): Data.Primitive = constant
