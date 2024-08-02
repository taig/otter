// package io.taig.otter

// import cats.syntax.all.*
// import cats.Invariant
// import io.taig.otter.validation.Violation
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Validation
// import cats.Id as Identity

// abstract class Dictionary[A] extends Codec[A]:
//   self =>

//   final override type Of = Data.Object[Element]

//   type Element <: Data

//   final override def modifyMetadata(f: Metadata => Metadata): Dictionary.Of[Optional, Element, A] = new Dictionary[A]:
//     export self.{decode, default, encode, Element, Optional}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Dictionary.Of[Optional, Element, A] = new Dictionary[A]:
//     export self.{encode, metadata, Element,Optional}
//     override def default: Option[A] = f(self.default)
//     override def decode(data: Data): Codec.Result[A] = (data, default) match
//       case (Data.Null, Some(default)) => default.valid
//       case _                          => self.decode(data)

//   final override def imap[B](f: A => B)(g: B => A): Dictionary.Of[Optional, Element, B] = ivalidate(Validation.lift(f))(g)

//   final def to[B](using evidence: Evidence.Product.Aux[B, A]): Dictionary.Of[Optional, Element, B] =
//     imap(evidence.from)(evidence.to)

//   final def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Dictionary.Of[Optional, Element, B] =
//     new Dictionary[B]:
//       export self.{metadata, Element,Optional}
//       override def default: Option[B] = self.default.flatMap(validation(_).toOption)
//       override def decode(data: Data): Codec.Result[B] =
//         self.decode(data).andThen(validation(_).leftMap(Violations.root))
//       override def encode(b: B): Out = self.encode(f(b))

//   final override def optional: Dictionary.Of[Data.Optional, Element, Option[A]] = new Dictionary[Option[A]]:
//       export self.{metadata, Element}
//       override type Optional[+a] = Data.Optional[a]
//       override def default: Option[Option[A]] = self.default.map(_.some)
//       override def decode(data: Data): Codec.Result[Option[A]] =
//         data.asValue.fold(default.flatten.valid)(self.decode(_).map(_.some))
//       override def encode(a: Option[A]): Out = a.map(self.encode).getOrElse(Data.Null)

// object Dictionary:
//   type Of[F[+a] <: Data.Optional[a], O <: Data, A] = Dictionary[A] { type Optional[+a] <: F[a]; type Element <: O }

//   def apply[A, B](
//       key: Codec.Of[Identity, Data.Primitive, A],
//       of: Codec[B]
//   ): Dictionary.Of[Identity, of.Out, Vector[(A, B)]] = new Dictionary[Vector[(A, B)]]:
//     override type Optional[+a] = a
//     override type Element = of.Out
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[Vector[(A, B)]] = None
//     override def decode(data: Data): Codec.Result[Vector[(A, B)]] = data.asObject
//       .toValid(Violations.rootNec(Violation(Constraint.Type("object"), actual = Data.String(data.name))))
//       .andThen(_.values.traverse { case (a, b) => (key.parseRequired(a), of.decode(b)).tupled })
//     override def encode(abs: Vector[(A, B)]): Data.Object[of.Out] =
//       Data.Object(abs.map { case (a, b) => (key.printRequired(a), of.encode(b)) })

//   // given [F[+a <: Data] <: Data.Optional[a], O <: Data]: Invariant[Dictionary[F, O, *]] with
//   //   override def imap[A, B](fa: Dictionary[F, O, A])(f: A => B)(g: B => A): Dictionary[F, O, B] =
//   //     fa.imap(f)(g)
