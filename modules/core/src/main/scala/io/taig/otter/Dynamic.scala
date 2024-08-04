package io.taig.otter

import cats.Id as Identity
import cats.syntax.all.*
import cats.Invariant
import io.taig.otter.Codec.Result
import io.taig.otter.Data.Required

abstract class Dynamic[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dynamic[F, O, A] = new Dynamic[F, O, A]:
    export self.{decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dynamic[F, O, A] = new Dynamic[F, O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dynamic[F, O, B] = new Dynamic[F, O, B]:
    export self.metadata
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[O] = self.encode(g(b))

  final override def optional: Dynamic[Data.Optional, O, Option[A]] = new Dynamic[Data.Optional, O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[O] = a.map(self.encode).getOrElse(Data.Null)

object Dynamic:
  def apply[A <: Data](f: Data => Codec.Result[A]): Dynamic[Data.Required, A, A] = new Dynamic[Data.Required, A, A]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = f(data)
    override def encode(a: A): A = a

  def apply[A <: Data](name: String)(f: Data => Option[A]): Dynamic[Identity, A, A] = Dynamic: data =>
    f(data).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))

  val Value: Dynamic[Identity, Data.Value, Data.Value] = Dynamic("value")(_.asValue)
  val Any: Dynamic[Data.Optional, Data.Value, Data] = Value.optional.imap(_.getOrElse(Data.Null))(_.asValue)
  val Object: Dynamic[Identity, Data.Object[?], Data.Object[?]] = Dynamic("object")(_.asObject)
  val Array: Dynamic[Identity, Data.Array[?], Data.Array[?]] = Dynamic("array")(_.asArray)
  val Primitive: Dynamic[Identity, Data.Primitive, Data.Primitive] = Dynamic("primitive")(_.asPrimitive)
  val Null: Dynamic[Identity, Data.Null.type, Data.Null.type] =
    Dynamic("null")(data => Option.when(data.isNull)(Data.Null))

  given [F[+a] <: Data.Optional[a], O <: Data.Value]: Invariant[Dynamic[F, O, *]] with
    override def imap[A, B](fa: Dynamic[F, O, A])(f: A => B)(g: B => A): Dynamic[F, O, B] = fa.imap(f)(g)
