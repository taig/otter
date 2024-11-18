package io.taig.otter

import cats.syntax.all.*

abstract class Dynamic[+O <: Data, A] extends Codec[O, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dynamic[O, A] = new Dynamic[O, A]:
    export self.{decode, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Dynamic[O, B] = new Dynamic[O, B]:
    export self.metadata
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): O = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Dynamic[O, B] = imap(convert.to)(convert.from)

object Dynamic:
  def apply[A <: Data](f: Data => Codec.Result[A]): Dynamic[A, A] = new Dynamic[A, A]:
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[A] = f(data)
    override def encode(a: A): A = a

  def apply[A <: Data](name: String)(f: Data => Option[A]): Dynamic[A, A] = Dynamic: data =>
    f(data).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))

  val Any: Dynamic[Data, Data] = Dynamic(_.valid)
  val Value: Dynamic[Data.Value, Data.Value] = Dynamic("value")(_.asValue)
  val Object: Dynamic[Data.Object[?], Data.Object[?]] = Dynamic("object")(_.asObject)
  val Array: Dynamic[Data.Array[?], Data.Array[?]] = Dynamic("array")(_.asArray)
  val Primitive: Dynamic[Data.Primitive, Data.Primitive] = Dynamic("primitive")(_.asPrimitive)
  val Number: Dynamic[Data.Number, Data.Number] = Dynamic("number")(_.asPrimitive.flatMap(_.asNumber))
  val Null: Dynamic[Data.Null, Data.Null] = Dynamic("null")(data => Option.when(data.isNull)(Data.Null))

  given [O <: Data]: CodecInvariant[Dynamic[O, *]] with
    override def imap[A, B](fa: Dynamic[O, A])(f: A => B)(g: B => A): Dynamic[O, B] = fa.imap(f)(g)

  given [O <: Data, A]: Metadata.Ops[Dynamic[O, A]] with
    extension (self: Dynamic[O, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Dynamic[O, A] = self.modifyMetadata(f)
