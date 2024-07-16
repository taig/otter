package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Codec.Result
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

abstract class Dynamic[A] extends Codec[Nothing, A]:
  self =>

  override def metadata(f: Metadata => Metadata): Dynamic[A] = new Dynamic[A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  override def default(f: Option[A] => Option[A]): Dynamic[A] = new Dynamic[A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  override def imap[B](f: A => B)(g: B => A): Dynamic[B] = new Dynamic[B]:
    export self.metadata
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data = self.encode(g(b))

  override def optional: Dynamic[Option[A]] = new Dynamic[Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(self.default.valid)(self.decode(_).map(_.some))
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)

object Dynamic:
  val Any: Dynamic[Data] = new Dynamic[Data]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data] = None
    override def decode(data: Data): Codec.Result[Data] = data.valid
    override def encode(a: Data): Data = a

  val Value: Dynamic[Data.Value] = new Dynamic[Data.Value]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data.Value] = None
    override def decode(data: Data): Result[Data.Value] =
      data.toObject.toValid(Violations.rootNec(Violation(Constraint.Type("value"), actual = Data.String(data.name))))
    override def encode(a: Data.Value): Data = a

  val Object: Dynamic[Data.Object] = new Dynamic[Data.Object]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data.Object] = None
    override def decode(data: Data): Result[Data.Object] =
      data.toObject.toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
    override def encode(a: Data.Object): Data = a

  val Array: Dynamic[Data.Array] = new Dynamic[Data.Array]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data.Array] = None
    override def decode(data: Data): Result[Data.Array] =
      data.toArray.toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))))
    override def encode(a: Data.Array): Data = a

  val Primitive: Dynamic[Data.Primitive] = new Dynamic[Data.Primitive]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data.Primitive] = None
    override def decode(data: Data): Result[Data.Primitive] =
      data.toPrimitive.toValid(
        Violations.rootNec(Violation(Constraint.Type("primitive"), actual = Data.String(data.name)))
      )
    override def encode(a: Data.Primitive): Data = a
