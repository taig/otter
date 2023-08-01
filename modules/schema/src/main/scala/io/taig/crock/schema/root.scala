//package io.taig.crock.schema
//
//import cats.data.Validated
//import cats.syntax.all.*
//import io.taig.crock.{Encoder, OpenApi}
//import io.taig.crock.syntax.*
//import io.taig.crock.validation.{Constraint, Validation, Violation}
//
////private[crock] def applyValidation[A: Encoder, B, C](validation: Validation[A, B, B, C], encode: B => OpenApi)(
////    b: B
////): Validated[Violations, C] = validation
////  .run(b)
////  .leftMap: violations =>
////    Violations.root(violations.map(_.mapReference(_.asOpenApi).mapActual(encode)))
