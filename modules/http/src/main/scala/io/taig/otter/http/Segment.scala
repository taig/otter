// package io.taig.otter.http

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.validation.{Constraint, Violation, Violations}
// import io.taig.otter.{Data, Evidence, Union, Value}

// sealed abstract class Segment[A]:
//   self =>
//   type Self[a] <: Segment[a]

//   def name: String

//   def imap[B](f: A => B)(g: B => A): Self[B]

//   final def /[B](segment: Segment[B])(using evidence: Evidence.Merge[A, B]): Path[evidence.Out] = toPath / segment
//   final def /(static: String): Path[A] = /(Segment.Static(static))

//   def matches(value: String): Boolean

//   def decode(value: String): Validated[Violations, A]
//   def encode(a: A): String

//   def print: String

//   final def toPath: Path[A] = Path(this)

// object Segment:
//   sealed abstract class Static[A](val name: String) extends Segment[A]:
//     self =>
//     final override type Self[a] = Segment.Static[a]
//     final override def imap[B](f: A => B)(g: B => A): Segment.Static[B] = new Static[B](name):
//       override def decode(value: String): Validated[Violations, B] = self.decode(value).map(f)
//       override def encode(b: B): String = self.encode(g(b))
//     final override def matches(value: String): Boolean = name === value
//     final override def print: String = name

//   object Static:
//     def apply(of: String): Segment.Static[Unit] = new Static[Unit](of):
//       override def decode(value: String): Validated[Violations, Unit] = Validated.cond(
//         matches(value),
//         (),
//         Violations.rootNec(Violation(Constraint.Equals(of), actual = Data.String(value)))
//       )
//       override def encode(a: Unit): String = of

//   sealed abstract class Parameter[A](
//       val name: String,
//       val codec: Value.Required[?] | Union.Required[?],
//       val description: Option[String]
//   ) extends Segment[A]:
//     self =>
//     final override type Self[a] = Segment.Parameter[a]
//     final def description(f: Option[String] => Option[String]): Segment.Parameter[A] =
//       new Parameter[A](name, codec, f(description)) { export self.* }
//     final def description(value: Option[String]): Segment.Parameter[A] = description(_ => value)
//     final def description(value: String): Segment.Parameter[A] = description(Some(value))
//     final override def imap[B](f: A => B)(g: B => A): Segment.Parameter[B] = new Parameter[B](name, codec, description):
//       override def decode(value: String): Validated[Violations, B] = self.decode(value).map(f)
//       override def encode(b: B): String = self.encode(g(b))
//     final override def matches(value: String): Boolean = true
//     final override def print: String = s"{$name}"

//   object Parameter:
//     def apply[A](parameter: String, of: Value.Required[A]): Segment.Parameter[A] =
//       new Parameter[A](parameter, of, None):
//         override def decode(value: String): Validated[Violations, A] = of.parse(value)
//         override def encode(a: A): String = of.print(a)

//     def apply[A](parameter: String, of: Union.Required.Of[Value.Required[?], A]): Segment.Parameter[A] =
//       new Parameter[A](parameter, of, None):
//         override def decode(value: String): Validated[Violations, A] = of.parse(value)
//         override def encode(a: A): String = of.print(a)
