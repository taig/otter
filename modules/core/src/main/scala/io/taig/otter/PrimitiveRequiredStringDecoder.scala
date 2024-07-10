// package io.taig.otter

// import io.taig.otter.validation.Violations
// import cats.syntax.all.*
// import io.taig.otter.validation.Violation

// object PrimitiveRequiredStringDecoder:
//   def apply[A](schema: Primitive.Required.Reader[A], value: String): Decoder.Result[Option[String], A] = schema match
//     case Primitive.Required.Reader.Transform(self, validation) => transform(self, validation, value)
//     case Primitive.Required.Transform(self, validation, _)     => transform(self, validation, value)
//     case Primitive.Required.Root(tpe) =>
//       TypeStringDecoder(tpe, value).toValid(
//         Violations.rootNec(Violation(Constraint.Type(name = tpe.toString), actual = value.some))
//       )

//   def transform[A, B, C, D](
//       self: Primitive.Required.Reader[A],
//       validation: SchemaValidation.Primitive[A, B, C, D],
//       value: String
//   ): Decoder.Result[Option[String], D] = PrimitiveRequiredStringDecoder(self, value).andThen: a =>
//     val encode = ValidationWriterValueStringEncoder.apply
//     validation(a).leftMap(_.map(_.bimap(_.map(encode), encode))).leftMap(Violations.root)
