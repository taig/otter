//package io.taig.openapi.schema
//
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.openapi.{Encoder, OpenApi}
//import io.taig.openapi.syntax.*
//import io.taig.openapi.validation.{Constraint, Validation}
//
//// TODO fan out into subclasses to have a Value for primitive variants
//sealed abstract class Dynamic[A] extends Schema[A]:
//  self =>
//
//  final override type Self[a] = Dynamic[a] { type Codec = self.Codec }
//
//  final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Self[C] =
//    Dynamic.Validate(this, validation, g)
//
//object Dynamic:
//  type Codec[A, B <: OpenApi] = Dynamic[A] { type Codec = B }
//  type Of[A <: OpenApi] = Codec[A, A]
//
//  final private case class Root[A <: OpenApi](
//      check: OpenApi => Option[A],
//      description: Option[String],
//      example: Option[A],
//      tpe: String
//  ) extends Dynamic[A]:
//    override type Codec = A
//    override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
//    override def modifyDescription(f: Option[String] => Option[String]): Dynamic.Of[A] =
//      copy(description = f(description))
//    override def modifyExample(f: Option[A] => Option[A]): Dynamic.Of[A] = copy(example = f(example))
//    override def decode(openapi: OpenApi): Validated[Violations, A] =
//      check(openapi).toValid(typeViolations(tpe, openapi))
//    override def encode(a: A): A = a
//
//  final private case class Validate[A, B: Encoder, C <: OpenApi, D](
//      dynamic: Dynamic.Codec[A, C],
//      validation: Validation[B, A, A, D],
//      g: D => A
//  ) extends Dynamic[D]:
//    override type Codec = C
//    override def constraints: Chain[Constraint[OpenApi]] =
//      dynamic.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
//    override def description: Option[String] = dynamic.description
//    override def example: Option[D] = dynamic.example.flatMap(validation.run(_).toOption)
//    override def modifyDescription(f: Option[String] => Option[String]): Dynamic.Codec[D, C] =
//      copy(dynamic = dynamic.modifyDescription(f))
//    override def modifyExample(f: Option[D] => Option[D]): Dynamic.Codec[D, C] =
//      copy(dynamic = dynamic.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
//    override def decode(openapi: OpenApi): Validated[Violations, D] =
//      dynamic.decode(openapi).andThen(applyValidation(validation, dynamic.encode))
//    override def encode(b: D): C = dynamic.encode(g(b))
//
//  def apply[A <: OpenApi](tpe: String)(f: OpenApi => Option[A]): Dynamic.Of[A] = Root(f, none, none, tpe)
