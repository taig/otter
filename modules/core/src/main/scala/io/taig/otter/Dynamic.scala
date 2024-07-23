package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import cats.Invariant
import io.taig.otter.Codec.Result
import io.taig.otter.Value.Required
import io.taig.otter.Codec.Required

abstract class Dynamic[+O <: Data, A] extends Codec[Nothing, A]:
  self =>

  final override def modifyMetadata(f: Metadata => Metadata): Dynamic[O, A] = ???

  final override def modifyDefault(f: Option[A] => Option[A]): Dynamic[O, A] = ???

  final override def imap[B](f: A => B)(g: B => A): Dynamic[O, B] = ???

  final override def optional: Dynamic[O, Option[A]] = new Dynamic[O, Option[A]]:
    export self.metadata
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def encode(a: Option[A]): Format[this.type] = a.map(self.encode).getOrElse(Data.Null)
    override def decode(data: Data): Codec.Result[Option[A]] = ???

object Dynamic:
  val Any: Dynamic[Data, Data] = new Dynamic[Data, Data]:
    override def metadata: Metadata = Metadata.Empty
    override def default: Option[Data] = None
    override def decode(data: Data): Codec.Result[Data] = data.valid
    override def encode(a: Data): Data = a

// val Value: Dynamic[Data.Value] = new Dynamic[Data.Value]:
//   override def metadata: Metadata = Metadata.Empty
//   override def default: Option[Data.Value] = None
//   override def decode(data: Data): Result[Data.Value] =
//     data.toObject.toValid(Violations.rootNec(Violation(Constraint.Type("value"), actual = Data.String(data.name))))
//   override def encode(a: Data.Value): Data = a

// val Object: Dynamic[Data.Object] = new Dynamic[Data.Object]:
//   override def metadata: Metadata = Metadata.Empty
//   override def default: Option[Data.Object] = None
//   override def decode(data: Data): Result[Data.Object] =
//     data.toObject.toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
//   override def encode(a: Data.Object): Data = a

// val Array: Dynamic[Data.Array[?]] = new Dynamic[Data.Array[?]]:
//   override def metadata: Metadata = Metadata.Empty
//   override def default: Option[Data.Array[?]] = None
//   override def decode(data: Data): Result[Data.Array[?]] =
//     data.toArray.toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))))
//   override def encode(a: Data.Array[?]): Data = a

// val Primitive: Dynamic[Data.Primitive] = new Dynamic[Data.Primitive]:
//   override def metadata: Metadata = Metadata.Empty
//   override def default: Option[Data.Primitive] = None
//   override def decode(data: Data): Result[Data.Primitive] = data.toPrimitive.toValid(
//     Violations.rootNec(Violation(Constraint.Type("primitive"), actual = Data.String(data.name)))
//   )
//   override def encode(a: Data.Primitive): Data = a

given [O <: Data]: Invariant[Dynamic[O, *]] with
  override def imap[A, B](fa: Dynamic[O, A])(f: A => B)(g: B => A): Dynamic[O, B] = fa.imap(f)(g)
