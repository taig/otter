//package io.taig.openapi.schema
//
//import cats.Eval
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.openapi.OpenApi
//import io.taig.validation.{Constraint, Validation, Violation}
//
//sealed abstract class Enumeration[A](
//    val constraints: Chain[Constraint[OpenApi]],
//    val metadata: Enumeration.Metadata[A],
//    val schema: Eval[Schema.Of[?, OpenApi.Primitive]],
//    val values: Eval[Set[OpenApi.Primitive]]
//) extends Value[A]:
//  self =>
//  final override type Self[a] = Enumeration[a]
//  final override type Metadata[a] = Enumeration.Metadata[a]
//
//  final override def copy(metadata: Enumeration.Metadata[A]): Enumeration[A] =
//    new Enumeration[A](constraints, metadata, schema, values) { export self.{decode, encode, parse, render} }
//
//  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Enumeration[B] = ???
//
//  final override def decode(openapi: OpenApi): Validated[Violations, A] = openapi match
//    case openapi: OpenApi.Primitive => decode(openapi)
//    case _                          => typeViolations("Primitive", openapi).invalid
//
//  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]
//
//object Enumeration:
//  final case class Metadata[A](description: Option[String], example: Option[A]) extends Schema.Metadata[A]:
//    override type Self[a] = Enumeration.Metadata[a]
//    override def map[B](f: A => B): Enumeration.Metadata[B] = copy(example = example.map(f))
//    override def flatMap[B](f: A => Option[B]): Enumeration.Metadata[B] = copy(example = example.flatMap(f))
//    override def updated(description: Option[String], example: Option[A]): Enumeration.Metadata[A] =
//      Metadata(description, example)
//
//  object Metadata:
//    def empty[A]: Enumeration.Metadata[A] = Metadata(None, None)
//
//  def apply[A, B](of: Eval[Value[A]], values: Set[B], mapping: B => A): Enumeration[B] =
//    val lookup: Eval[A => Option[B]] = Eval.later(values.map(b => mapping(b) -> b).toMap.get(_))
//    val openapis = of.map(schema => values.map(mapping).map(schema.encode))
//
//    new Enumeration[B](Chain.empty, Metadata.empty, of, openapis):
//      override def decode(openapi: OpenApi.Primitive): Validated[Violations, B] =
//        of.value
//          .decode(openapi)
//          .andThen: key =>
//            Validated.fromOption(
//              lookup.value(key), {
//                val references = OpenApi.fromList(openapis.value.toList)
//                val constraint = Constraint("enumeration", references.some)
//                Violations.rootNec(Violation(constraint, of.value.encode(key)))
//              }
//            )
//
//      override def encode(b: B): OpenApi.Primitive = of.value.encode(mapping(b))
//
//      override def parse(value: String): Validated[Violations, B] = of.value.parse(value).andThen(???)
//
//      override def render(b: B): String = of.value.render(mapping(b))
