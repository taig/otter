// package io.taig.otter.component

// import cats.Invariant
// import cats.syntax.all.*
// import io.taig.Undefined
// import io.taig.otter.Constraint
// import io.taig.otter.operation.StringOperation
// import io.taig.validation.Validation
// import io.taig.validation.std

// import java.util.UUID
// import java.util.regex.Pattern

// trait StringComponent[+Self[_]: Invariant](using operation: StringOperation[Self]):
//   self =>

//   def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String] =
//     operation.string(validation)

//   final val string: Self[String] = operation.string(Validation.valid)

//   def string(
//       minimum: Undefined.Or[Int] = Undefined,
//       maximum: Undefined.Or[Int] = Undefined,
//       pattern: Undefined.Or[Pattern] = Undefined
//   ): Self[String] =
//     val validation = (
//       minimum.map(minimum => std.text.minimum[String](reference = minimum.toLong)).toList ++
//         maximum.map(maximum => std.text.maximum[String](reference = maximum.toLong)).toList ++
//         pattern.map(std.text.matches[String]).toList
//     ).foldLeft[Validation[Constraint.Primitive.Text, String]](Validation.valid)(_ & _)

//     string(validation)

//   extension (x: string.type)
//     def required(
//         maximum: Undefined.Or[Int] = Undefined,
//         matches: Undefined.Or[Pattern] = Undefined
//     ): Self[String] = self.string(minimum = 1, maximum, matches)

//     def required: Self[String] = required()

//     def nonEmpty: Self[Option[String]] = self.string.imap(_.some.filter(_.nonEmpty))(_.getOrElse(""))

//   def parser[A](name: String)(f: String => Either[String, A])(g: A => String): Self[A] =
//     ??? // operation.parser(name, decode = f, encode = g)

//   val uuid: Self[UUID] = parser(name = "uuid") { value =>
//     Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
//   }(_.show)

//   val pattern: Self[Pattern] = string.imap(Pattern.compile)(_.pattern)
