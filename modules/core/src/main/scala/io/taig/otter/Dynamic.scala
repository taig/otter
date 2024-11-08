package io.taig.otter

import cats.syntax.all.*

abstract class Dynamic[+F[+a] <: Data.Nullable[a], +O <: Data, A] extends Codec[F, O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dynamic[F, O, A] = new Dynamic[F, O, A]:
    export self.{decode, default, encode, isNullable}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Dynamic[F, O, A] = new Dynamic[F, O, A]:
    export self.{encode, metadata}
    override def default: Option[A] = f(self.default)
    override def isNullable: Boolean = default.nonEmpty
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Dynamic[F, O, B] = new Dynamic[F, O, B]:
    export self.{isNullable, metadata}
    override def default: Option[B] = self.default.map(f)
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): F[O] = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Dynamic[F, O, B] = imap(convert.to)(convert.from)

  final override def nullable: Dynamic[Data.Nullable, O, Option[A]] = new Dynamic[Data.Nullable, O, Option[A]]:
    export self.metadata
    override def isNullable: Boolean = true
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Nullable[O] = a.map(self.encode).getOrElse(Data.Null)

object Dynamic:
  def apply[A <: Data](f: Data => Codec.Result[A]): Dynamic[Data.Required, A, A] = new Dynamic[Data.Required, A, A]:
    override def isNullable: Boolean = false
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[A] = None
    override def decode(data: Data): Codec.Result[A] = f(data)
    override def encode(a: A): A = a

  def apply[A <: Data](name: String)(f: Data => Option[A]): Dynamic[Data.Required, A, A] = Dynamic: data =>
    f(data).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))

  val Value: Dynamic[Data.Required, Data.Value, Data.Value] = Dynamic("value")(_.asValue)
  val Any: Dynamic[Data.Nullable, Data.Value, Data] = Value.nullable.imap(_.getOrElse(Data.Null))(_.asValue)
  val Object: Dynamic[Data.Required, Data.Object[?], Data.Object[?]] = Dynamic("object")(_.asObject)
  val Array: Dynamic[Data.Required, Data.Array[?], Data.Array[?]] = Dynamic("array")(_.asArray)
  val Primitive: Dynamic[Data.Required, Data.Primitive, Data.Primitive] = Dynamic("primitive")(_.asPrimitive)
  val Number: Dynamic[Data.Required, Data.Number, Data.Number] = Dynamic("number")(_.asPrimitive.flatMap(_.asNumber))
  val Null: Dynamic[Data.Required, Data.Null.type, Data.Null.type] =
    Dynamic("null")(data => Option.when(data.isNull)(Data.Null))

  given [F[+a] <: Data.Nullable[a], O <: Data]: CodecInvariant[Dynamic[F, O, *]] with
    override def imap[A, B](fa: Dynamic[F, O, A])(f: A => B)(g: B => A): Dynamic[F, O, B] = fa.imap(f)(g)

  given [F[+a] <: Data.Nullable[a], O <: Data, A]: Metadata.Ops[Dynamic[F, O, A]] with
    extension (self: Dynamic[F, O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Dynamic[F, O, A] = self.modifyMetadata(f)
