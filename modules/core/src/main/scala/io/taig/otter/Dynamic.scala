package io.taig.otter

import cats.syntax.all.*
import cats.Invariant
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result

abstract class Dynamic[+O <: Data, A] extends Codec[O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dynamic[O, A] = new Dynamic[O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dynamic[O, A] = new Dynamic[O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dynamic[O, B] = new Dynamic[O, B]:
    export self.metadata
    override def default: Option[B] = self.default.map(f)
    override def encode(b: B): O = self.encode(g(b))
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)

  final override def optional: Dynamic[Data.Optional[O], Option[A]] = new Dynamic[Data.Optional[O], Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))

object Dynamic:
  def apply[A <: Data](f: Data => Codec.Result[A]): Dynamic[A, A] = new Dynamic[A, A]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = ???
    override def encode(a: A): A = a

  def apply[A <: Data](name: String)(f: Data => Option[A]): Dynamic[A, A] =
    Dynamic(data =>
      f(data).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
    )

  val Any: Dynamic[Data, Data] = Dynamic(_.valid)
  val Value: Dynamic[Data.Value, Data.Value] = Dynamic("value")(_.toValue)
  val Object: Dynamic[Data.Object[?], Data.Object[?]] = Dynamic("object")(_.toObject)
  val Array: Dynamic[Data.Array[?], Data.Array[?]] = Dynamic("array")(_.toArray)
  val Primitive: Dynamic[Data.Primitive, Data.Primitive] = Dynamic("primitive")(_.toPrimitive)

given [O <: Data]: Invariant[Dynamic[O, *]] with
  override def imap[A, B](fa: Dynamic[O, A])(f: A => B)(g: B => A): Dynamic[O, B] = fa.imap(f)(g)
