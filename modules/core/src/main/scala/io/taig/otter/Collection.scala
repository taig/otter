// package io.taig.otter

// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Validation
// import io.taig.otter.validation.Violation
// import cats.Id as Identity
// import io.taig.otter.Codec.Result

// sealed abstract class Collection[A] extends Codec[A]:
//   self =>

//   final override type Of = Data.Array[Element]

//   type Element <: Data

//   def codec: Codec[?]

//   final override def modifyMetadata(f: Metadata => Metadata): Collection.Of[Optional, Element, A] = new Collection[A]:
//     export self.{codec, decode, default, encode, Element, Optional}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Collection.Of[Optional, Element, A] = new Collection[A]:
//     export self.{codec, encode, metadata, Element, Optional}
//     override def default: Option[A] = f(self.default)
//     override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
//       case (None, Some(default)) => default.valid
//       case _                     => self.decode(data)

//   final override def imap[B](f: A => B)(g: B => A): Collection.Of[Optional, Element, B] = ivalidate(Validation.lift(f))(g)

//   final def to[B](using evidence: Evidence.Product.Aux[B, A]): Collection.Of[Optional, Element, B] = imap(evidence.from)(evidence.to)

//   final override def ivalidate[B](validation: CodecValidation[Of, A, B])(f: B => A): Collection.Of[Optional, Element, B] =
//     new Collection[B]:
//       export self.{codec, metadata, Element, Optional}
//       override def default: Option[B] = self.default.flatMap(validation(_).toOption)
//       override def decode(data: Option[Vector[Data]]): Codec.Result[B] =
//         self.decode(data).andThen(validation(_).leftMap(Violations.root))
//       override def encode(b: B): self.Out = self.encode(f(b))

//   override def optional: Collection.Of[Data.Optional, Element, Option[A]] = new Collection[Option[A]]:
//     export self.{codec, metadata, Element}
//     override type Optional[+a] = Data.Optional[a]
//     override def default: Option[Option[A]] = self.default.map(_.some)
//     override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] =
//       data.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
//     override def encode(a: Option[A]): Data.Optional[self.Of] = a.map(self.encode).getOrElse(Data.Null)

//   override def decode(data: Data): Codec.Result[A] = data match
//     case Data.Array(values) => decode(values.some)
//     case Data.Null          => decode(none)
//     case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

//   def decode(data: Option[Vector[Data]]): Codec.Result[A]

// object Collection:
//   type Of[F[+a] <: Data.Optional[a], O <: Data, A] = Collection[A] { type Optional[+a] <: F[a]; type Element <: O }

//   def apply[A](of: Codec[A]): Collection.Of[Identity, of.Out, Vector[A]] =
//     new Collection[Vector[A]]:
//       override type Optional[+a] = a
//       override type Element = of.Out
//       override def codec: Codec[?] = of
//       override def metadata: Metadata = Metadata.Empty
//       override def default: Option[Vector[A]] = None
//       override def decode(data: Option[Vector[Data]]): Codec.Result[Vector[A]] = data
//         .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
//         .andThen(_.zipWithIndex.traverse { case (data, index) => of.decode(data).leftMap(index /: _) })
//       override def encode(as: Vector[A]): Data.Array[of.Out] = Data.Array(as.map(of.encode))

//   // given invariant[F[+a <: Data] <: Data.Optional[a], O <: Data]
//   //     : ValidationInvariant[[_] =>> Constraint.Collection, Collection[F, O, *]] with
//   //   extension [A](self: Collection[F, O, A])
//   //     override def ivalidate[B](validation: CodecValidation.Collection[A, B])(f: B => A): Collection[F, O, B] =
//   //       self.ivalidate(validation)(f)
