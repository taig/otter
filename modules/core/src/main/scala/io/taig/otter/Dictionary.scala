// package io.taig.otter

// import cats.syntax.all.*
// import io.taig.otter.Codec.Result
// import cats.data.Chain
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation

// abstract class Dictionary[+O, A] extends Codec[O, A]:
//   self =>

//   override def imap[B](f: A => B)(g: B => A): Dictionary[O, B] = new Dictionary[O, B]:
//     export self.metadata
//     override def decodeObject(data: Option[Data.Object]): Codec.Result[Data, B] =
//       self.decodeOption(data).map(f)
//     override def encodeObject(b: B): Option[Data.Object] = self.encodeObject(g(b))

//   override def optional: Dictionary[O, Option[A]] = new Dictionary[O, Option[A]]:
//     export self.metadata
//     override def decodeObject(data: Option[Data.Object]): Codec.Result[Data, Option[A]] =
//       data.fold(none.valid)(_ => self.decodeOption(data).map(_.some))
//     override def encodeObject(a: Option[A]): Option[Data.Object] = a.flatMap(self.encodeObject)

//   override def update(f: Metadata => Metadata): Dictionary[O, A] = new Dictionary[O, A]:
//     export self.{decodeObject, encodeObject}
//     override def metadata: Metadata = f(self.metadata)

//   final override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, A] = data match
//     case Some(data: Data.Object) => decodeObject(Some(data))
//     case Some(data) => Violations.rootNec(Violation(Constraint.Type("object"), Data.String(data.name))).invalid
//     case None       => decodeObject(None)

//   def decodeObject(data: Option[Data.Object]): Codec.Result[Data, A]

//   final override def encodeOption(a: A): Option[Data.Value] = encodeObject(a)

//   def encodeObject(a: A): Option[Data.Object]

// object Dictionary:
//   def apply[A, B](key: Value.Required[?, A], value: Codec[?, B]): Dictionary[value.type, Chain[(A, B)]] =
//     new Dictionary[value.type, Chain[(A, B)]]:
//       override def metadata: Metadata = Metadata.Empty
//       override def decodeObject(data: Option[Data.Object]): Result[Data, Chain[(A, B)]] = data
//         .toValid(Violations.rootNec(Violation(Constraint.Type("object"), Data.String("null"))))
//         .andThen(_.values.traverse { case (a, b) =>
//           (
//             key.parseValue(a).leftMap(_.map(_.bimap(_.map(Data.String.apply), Data.String.apply))),
//             value.decode(b)
//           ).tupled
//         })
//       override def encodeObject(abs: Chain[(A, B)]): Option[Data.Object] =
//         Data.Object(abs.map { case (a, b) => (key.printValue(a), value.encode(b)) }).some
