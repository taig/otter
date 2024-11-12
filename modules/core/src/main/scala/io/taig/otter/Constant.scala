package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import cats.data.Validated

sealed abstract class Constant[A] extends Codec[Data.Primitive, A]:
  self =>

  override def modifyMetadata(f: Metadata => Metadata): Constant[A] = new Constant[A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def modifyDefault(f: Option[A] => Option[A]): Constant[A] = new Constant[A]:
    export self.{decode, encode, metadata}
    override def default: Option[A] = f(self.default)

  override def imap[B](f: A => B)(g: B => A): Constant[B] = new Constant[B]:
    export self.metadata
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Primitive = self.encode(g(b))

  override def to[B](using convert: Convert[A, B]): Constant[B] = imap(convert.to)(convert.from)

object Constant:
  def apply[A](codec: Codec[Data.Primitive, A], value: A): Constant[value.type] = new Constant[value.type]:
    val constant = codec.encode(value)

    override def metadata: Metadata = Metadata.Empty
    override def default: Option[value.type] = none
    override def decode(data: Data): Codec.Result[value.type] =
      Validated.cond(data === constant, value, Violations.rootNec(Violation.tpe(constant.plain, data)))

    override def encode(a: value.type): Data.Primitive = constant
