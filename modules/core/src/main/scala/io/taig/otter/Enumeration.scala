// package io.taig.otter

// import io.taig.enumeration.ext.Mapping
// import io.taig.otter.Codec.Result
// import cats.syntax.all.*
// import io.taig.otter.validation.Violations
// import io.taig.otter.validation.Violation

// abstract class Enumeration[+O, A] extends Value[O, A]:
//   self =>

//   override def imap[B](f: A => B)(g: B => A): Enumeration[O, B] = new Enumeration[O, B]:
//     export self.metadata
//     override def decodeOption(data: Option[Data.Value]): Result[Data, B] =
//       self.decodeOption(data).map(f)
//     override def encodeOption(b: B): Option[Data.Value] = self.encodeOption(g(b))
//     override def parse(value: Option[String]): Result[String, B] = self.parse(value).map(f)
//     override def print(b: B): Option[String] = self.print(g(b))

//   final override def optional: Enumeration[O, Option[A]] = new Enumeration[O, Option[A]]:
//     export self.metadata
//     override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, Option[A]] =
//       data.fold(none.valid)(_ => self.decodeOption(data).map(_.some))
//     override def encodeOption(a: Option[A]): Option[Data.Value] = a.flatMap(self.encodeOption)
//     override def parse(value: Option[String]): Codec.Result[String, Option[A]] =
//       value.fold(none.valid)(_ => self.parse(value).map(_.some))
//     override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

//   override def update(f: Metadata => Metadata): Enumeration[O, A] = new Enumeration[O, A]:
//     export self.{decodeOption, encodeOption, parse, print}
//     override def metadata: Metadata = f(self.metadata)

// object Enumeration:
//   abstract class Required[+O, A] extends Enumeration[O, A], Value.Required[O, A]:
//     self =>

//     final override def imap[B](f: A => B)(g: B => A): Enumeration.Required[O, B] = new Enumeration.Required[O, B]:
//       export self.metadata
//       override def decodeValue(data: Data.Value): Codec.Result[Data, B] = self.decodeValue(data).map(f)
//       override def encodeValue(b: B): Data.Value = self.encodeValue(g(b))
//       override def parseValue(value: String): Codec.Result[String, B] =
//         self.parseValue(value).map(f)
//       override def printValue(b: B): String = self.printValue(g(b))

//     final override def update(f: Metadata => Metadata): Enumeration.Required[O, A] = new Enumeration.Required[O, A]:
//       export self.{decodeValue, encodeValue, parseValue, printValue}
//       override def metadata: Metadata = f(self.metadata)

//   def apply[A, B](of: Value.Required[?, A], mapping: Mapping[B, A]): Enumeration.Required[of.type, B] =
//     new Enumeration.Required[of.type, B]:
//       override def metadata: Metadata = Metadata.Empty

//       override def decodeValue(data: Data.Value): Codec.Result[Data, B] = of
//         .decodeValue(data)
//         .andThen: a =>
//           mapping
//             .unapply(a)
//             .toValid(Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(encode)), actual = data)))

//       override def encodeValue(b: B): Data.Value = of.encodeValue(mapping(b))

//       override def parseValue(value: String): Codec.Result[String, B] = of
//         .parseValue(value)
//         .andThen: a =>
//           mapping
//             .unapply(a)
//             .toValid(Violations.rootNec(Violation(Constraint.OneOf(mapping.values.map(printValue)), actual = value)))

//       override def printValue(b: B): String = of.printValue(mapping(b))
