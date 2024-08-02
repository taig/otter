// package io.taig.otter

// import cats.Id as Identity
// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import cats.Invariant
// import io.taig.otter.Codec.Result

// abstract class Dynamic[A] extends Codec[A]:
//   self =>

//   final override def modifyMetadata(f: Metadata => Metadata): Dynamic.Of[Optional, Of, A] = new Dynamic[A]:
//     export self.{decode, default, encode, Of,Optional}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Dynamic.Of[Optional, Of, A] = new Dynamic[A]:
//     export self.{encode, metadata, Of,Optional}
//     override def default: Option[A] = f(self.default)
//     override def decode(data: Data): Codec.Result[A] = (data, default) match
//       case (Data.Null, Some(default)) => default.valid
//       case _                          => self.decode(data)

//   final override def imap[B](f: A => B)(g: B => A): Dynamic.Of[Optional, Of, B] = new Dynamic[B]:
//     export self.{metadata, Of, Optional}
//     override def default: Option[B] = self.default.map(f)
//     override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
//     override def encode(b: B): self.Out = self.encode(g(b))

//   override def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Dynamic.Of[Optional, Of, B] = ???

//   final override def optional: Dynamic.Of[Data.Optional, Of, Option[A]] = new Dynamic[Option[A]]:
//     export self.{metadata, Of}
//     override type Optional[+a] = Data.Optional[a]
//     override def default: Option[Option[A]] = self.default.map(_.some)
//     override def decode(data: Data): Codec.Result[Option[A]] =
//       data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
//     override def encode(a: Option[A]): Data.Optional[self.Out] = a.map(self.encode).getOrElse(Data.Null)

// object Dynamic:
//   type Of[F[+a] <: Data.Optional[a], O <: Data, A] = Dynamic[A] { type Optional[+a] <: F[a]; type Of <: O }

//   def apply[A <: Data](f: Data => Codec.Result[A]): Dynamic.Of[Identity, A, A] = new Dynamic[A]:
//     override type Optional[+a] = a
//     override type Of = A
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[A] = None
//     override def decode(data: Data): Codec.Result[A] = f(data)
//     override def encode(a: A): A = a

//   def apply[A <: Data](name: String)(f: Data => Option[A]): Dynamic.Of[Identity, A, A] = Dynamic: data =>
//     f(data).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))

//   val Value: Dynamic.Of[Identity, Data.Value, Data.Value] = Dynamic("value")(_.asValue)
//   val Any: Dynamic.Of[Data.Optional, Data.Value, Data] = Value.optional.imap(_.getOrElse(Data.Null))(_.asValue)
//   val Object: Dynamic.Of[Identity, Data.Object[?], Data.Object[?]] = Dynamic("object")(_.asObject)
//   val Array: Dynamic.Of[Identity, Data.Array[?], Data.Array[?]] = Dynamic("array")(_.asArray)
//   val Primitive: Dynamic.Of[Identity, Data.Primitive, Data.Primitive] = Dynamic("primitive")(_.asPrimitive)
//   val Null: Dynamic.Of[Identity, Data.Null.type, Data.Null.type] =
//     Dynamic("null")(data => Option.when(data.isNull)(Data.Null))

//   given [F[+a] <: Data.Optional[a], O <: Data]: Invariant[Dynamic.Of[F, O, *]] with
//     override def imap[A, B](fa: Dynamic.Of[F, O, A])(f: A => B)(g: B => A): Dynamic.Of[F, O, B] = fa.imap(f)(g)
