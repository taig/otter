// package io.taig.otter

// import cats.syntax.all.*
// import cats.Invariant
// import io.taig.enumeration.ext.Mapping
// import io.taig.otter.Codec.Result
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation
// import cats.Id as Identity

// abstract class Enumeration[A] extends Codec[A]:
//   self =>

//   final override type Of = Data.Primitive

//   def codec: Codec[?]

//   override def modifyMetadata(f: Metadata => Metadata): Enumeration.Of[Optional, A] = new Enumeration[A]:
//     export self.{codec, decode, default, encode, Optional}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Enumeration.Of[Optional, A] = new Enumeration[A]:
//     export self.{codec, encode, metadata, Optional}
//     override def default: Option[A] = f(self.default)
//     override def decode(data: Data): Codec.Result[A] = (data, default) match
//       case (Data.Null, Some(default)) => default.valid
//       case _                          => self.decode(data)

//   override def imap[B](f: A => B)(g: B => A): Enumeration.Of[Optional, B] = new Enumeration[B]:
//     export self.{codec, metadata, Optional}
//     override def default: Option[B] = self.default.map(f)
//     override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
//     override def encode(b: B): Out = self.encode(g(b))

//   override def ivalidate[B](validation: CodecValidation[Data.Primitive, A, B])(f: B => A): Enumeration.Of[Optional, B] =
//     ???

//   final override def optional: Enumeration.Of[Data.Optional, Option[A]] = new Enumeration[Option[A]]:
//     export self.{codec, metadata}
//     override type Optional[+a] = Data.Optional[a]
//     override def default: Option[Option[A]] = self.default.map(_.some)
//     override def decode(data: Data): Codec.Result[Option[A]] =
//       data.asValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
//     override def encode(a: Option[A]): Out = a.map(self.encode).getOrElse(Data.Null)

// object Enumeration:
//   type Of[F[+a] <: Data.Optional[a], A] = Enumeration[A] { type Optional[+a] = F[a] }

//   def apply[A, B](of: Codec.Of[Identity, Data.Primitive, A], mapping: Mapping[B, A]): Enumeration.Of[Identity, B] =
//     new Enumeration[B]:
//       override type Optional[+a] = a
//       override def codec: Codec[?] = of
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[B] = None
//       override def decode(data: Data): Codec.Result[B] = of
//         .decode(data)
//         .andThen: a =>
//           mapping
//             .unapply(a)
//             .toValid(
//               Violations.rootNec(Violation(Constraint.Primitive.OneOf(mapping.values.map(encode)), actual = data))
//             )
//       override def encode(b: B): Data.Primitive = of.encode(mapping(b))

//   // given [F[+a] <: Data.Optional[a]]: Invariant[Enumeration[F, *]] with
//   //   override def imap[A, B](fa: Enumeration[F, A])(f: A => B)(g: B => A): Enumeration[F, B] = fa.imap(f)(g)
