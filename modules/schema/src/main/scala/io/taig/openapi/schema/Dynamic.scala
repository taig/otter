//package io.taig.openapi.schema
//
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.openapi.OpenApi
//import io.taig.validation.{Constraint, Validation}
//
//sealed abstract class Dynamic[A](val constraints: Chain[Constraint[OpenApi]], val metadata: Dynamic.Metadata[A])
//    extends Schema[A]:
//  self =>
//
//  final override type Self[a] = Dynamic[a] { type Codec = self.Codec }
//  final override type Metadata[a] = Dynamic.Metadata[a]
//
//  final override def copy(metadata: Dynamic.Metadata[A]): Self[A] =
//    new Dynamic[A](constraints, metadata) { export self.{decode, encode, Codec} }
//
//  final override def ivalidate[B](validation: Validation[A, A, A, B])(g: B => A): Self[B] = new Dynamic[B](
//    constraints ++ validation.constraints.map(_.map(self.encode)),
//    metadata.flatMap(validation.run(_).toOption)
//  ):
//    override type Codec = self.Codec
//    override def decode(openapi: OpenApi): Validated[Violations, B] =
//      self.decode(openapi).andThen(andThenValidate(validation, self.encode))
//    override def encode(b: B): self.Codec = self.encode(g(b))
//
//object Dynamic:
//  type Codec[A, B <: OpenApi] = Dynamic[A] { type Codec = B }
//  type Of[A <: OpenApi] = Codec[A, A]
//
//  final case class Metadata[A](description: Option[String], example: Option[A]) extends Schema.Metadata[A]:
//    override type Self[a] = Dynamic.Metadata[a]
//    override def map[B](f: A => B): Dynamic.Metadata[B] = copy(example = example.map(f))
//    override def flatMap[B](f: A => Option[B]): Dynamic.Metadata[B] = copy(example = example.flatMap(f))
//    override def updated(description: Option[String], example: Option[A]): Dynamic.Metadata[A] =
//      Metadata(description, example)
//
//  object Metadata:
//    def empty[A]: Dynamic.Metadata[A] = Metadata(None, None)
//
//  def apply[A <: OpenApi](tpe: String)(f: OpenApi => Option[A]): Dynamic.Of[A] =
//    new Dynamic[A](Chain.empty, Metadata.empty):
//      override type Codec = A
//      override def decode(openapi: OpenApi): Validated[Violations, A] = f(openapi).toValid(typeViolations(tpe, openapi))
//      override def encode(a: A): A = a
