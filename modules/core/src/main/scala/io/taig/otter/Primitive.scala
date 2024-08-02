// package io.taig.otter

// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import io.taig.otter.validation.Validation
// import cats.Id as Identity

// sealed abstract class Primitive[A] extends Codec[A]:
//   self =>

//   final override type Of = Data.Primitive

//   final override def modifyMetadata(f: Metadata => Metadata): Primitive.Of[Optional, A] = new Primitive[A]:
//     export self.{decode, default, encode, Optional}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Primitive.Of[Optional, A] = new Primitive[A]:
//     export self.{encode, metadata, Optional}
//     override def default: Option[A] = f(self.default)
//     override def decode(data: Data): Codec.Result[A] = (data, default) match
//       case (Data.Null, Some(default)) => default.valid
//       case _                          => self.decode(data)

//   final override def imap[B](f: A => B)(g: B => A): Primitive.Of[Optional, B] = ivalidate(Validation.lift(f))(g)

//   final def to[B](using evidence: Evidence.Product.Aux[B, A]): Primitive.Of[Optional, B] =
//     imap(evidence.from)(evidence.to)

//   final def ivalidate[B](validation: CodecValidation[Data.Primitive, A, B])(f: B => A): Primitive.Of[Optional, B] =
//     new Primitive[B]:
//       export self.{metadata, Optional}
//       override def default: Option[B] = self.default.flatMap(validation(_).toOption)
//       override def encode(b: B): Out = self.encode(f(b))
//       override def decode(data: Data): Codec.Result[B] =
//         self.decode(data).andThen(validation(_).leftMap(Violations.root))

//   final override def optional: Primitive.Of[Data.Optional, Option[A]] = new Primitive[Option[A]]:
//     export self.metadata
//     override type Optional[+a] = Data.Optional[a]
//     override def default: Option[Option[A]] = self.default.map(_.some)
//     override def decode(data: Data): Codec.Result[Option[A]] =
//       data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
//     override def encode(a: Option[A]): Data.Optional[Data.Primitive] = a.map(self.encode).getOrElse(Data.Null)

// object Primitive:
//   type Of[F[+a] <: Data.Optional[a], A] = Primitive[A] { type Optional[+a] = F[a] }

//   def apply[A](tpe: Type[A]): Primitive.Of[Identity, A] = new Primitive[A]:
//     override type Optional[+a] = a
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[A] = None
//     override def decode(data: Data): Codec.Result[A] = data.asPrimitive
//       .flatMap(tpe.decode)
//       .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.name), actual = Data.String(data.name))))
//     override def encode(a: A): Data.Primitive = tpe.encode(a)

//   // given [F[+a] <: Data.Optional[a]]: ValidationInvariant[Constraint.Primitive, Primitive[F, *]] with
//   //   extension [A](self: Primitive[F, A])
//   //     override def ivalidate[B](validation: CodecValidation[Data.Primitive, A, B])(f: B => A): Primitive[F, B] =
//   //       self.ivalidate(validation)(f)

//   // given [F[+a] <: Data.Optional[a], A]: Metadata.Ops[Primitive[F, A]] = new Metadata.Ops[Primitive[F, A]]:
//   //   extension (self: Primitive[F, A])
//   //     override def metadata: Metadata = self.metadata
//   //     override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A] = self.modifyMetadata(f)
