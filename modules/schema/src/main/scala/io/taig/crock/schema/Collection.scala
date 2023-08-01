//package io.taig.crock.schema
//
//import cats.Eval
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.crock.{Encoder, OpenApi}
//import io.taig.crock.schema.applyValidation
//import io.taig.crock.syntax.*
//import io.taig.crock.validation.{Constraint, Validation}
//
//sealed abstract class Collection[A] extends Schema[A]:
//  self =>
//
//  type Of <: OpenApi
//  override type Self[a] <: Collection[a] { type Self[a] = self.Self[a]; type Of = self.Of }
//  final override type Codec = OpenApi.Array[Of]
//
//  def schema: Eval[Schema.Of[?, Of]]
//
//  final override def decode(crock: OpenApi): Validated[Violations, A] = crock match
//    case crock: OpenApi.Array[?] => decode(crock)
//    case crock                   => typeViolations("Array", crock).invalid
//
//  def decode(crock: OpenApi.Array[?]): Validated[Violations, A]
//
//object Collection:
//  type Of[A, B <: OpenApi] = Collection[A] { type Of = B }
//
//  abstract class Object[A] extends Collection[A]:
//    override type Of <: OpenApi
//    final override type Self[a] = Collection.Object.Of[a, Of]
//    final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(
//        g: C => A
//    ): Collection.Object.Of[C, Of] =
//      Object.Validate(this, validation, g)
//
//  object Object:
//    type Of[A, B <: OpenApi] = Collection.Object[A] { type Of = B }
//
//    final private case class Root[A, B <: OpenApi](
//        description: Option[String],
//        example: Option[Vector[A]],
//        schema: Eval[Schema.Of[A, B]]
//    ) extends Collection.Object[Vector[A]]:
//      override type Of = B
//      override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
//      override def modifyDescription(f: Option[String] => Option[String]): Object.Of[Vector[A], B] =
//        copy(description = f(description))
//      override def modifyExample(f: Option[Vector[A]] => Option[Vector[A]]): Object.Of[Vector[A], B] =
//        copy(example = f(example))
//      override def decode(crock: OpenApi.Array[?]): Validated[Violations, Vector[A]] =
//        crock.toVector.zipWithIndex.traverse { case (crock, index) =>
//          schema.value.decode(crock).leftMap(_.modifyHistory(index /: _))
//        }
//      override def encode(a: Vector[A]): OpenApi.Array[B] = OpenApi.Array(a.map(schema.value.encode))
//
//    final private case class Validate[A, B: Encoder, C <: OpenApi, D](
//        collection: Collection.Object.Of[A, C],
//        validation: Validation[B, A, A, D],
//        g: D => A
//    ) extends Collection.Object[D]:
//      export collection.{description, schema}
//      override type Of = C
//      override def example: Option[D] = collection.example.flatMap(validation.run(_).toOption)
//      override def constraints: Chain[Constraint[OpenApi]] =
//        collection.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
//      override def modifyDescription(f: Option[String] => Option[String]): Collection.Object.Of[D, C] =
//        copy(collection = collection.modifyDescription(f))
//      override def modifyExample(f: Option[D] => Option[D]): Collection.Object.Of[D, C] =
//        copy(collection = collection.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
//      override def decode(crock: OpenApi.Array[?]): Validated[Violations, D] =
//        collection.decode(crock).andThen(applyValidation(validation, collection.encode))
//      override def encode(c: D): OpenApi.Array[C] = collection.encode(g(c))
//
//    def apply[A, B <: OpenApi](schema: Eval[Schema.Of[A, B]]): Collection.Object.Of[Vector[A], B] =
//      Root(none, none, schema)
//
//  abstract class Value[A] extends Collection[A]:
//    final override type Of = OpenApi.Primitive
//    final override type Self[a] = Collection.Value[a]
//    override def schema: Eval[Schema.Value[?]]
//    final override def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Collection.Value[C] =
//      Value.Validate(this, validation, g)
//    def parse(values: Vector[String]): Validated[Violations, A]
//    def render(a: A): Vector[String]
//
//  object Value:
//    final private case class Root[A](
//        description: Option[String],
//        example: Option[Vector[A]],
//        schema: Eval[Schema.Value[A]]
//    ) extends Collection.Value[Vector[A]]:
//      override def constraints: Chain[Constraint[OpenApi]] = Chain.empty
//      override def modifyDescription(f: Option[String] => Option[String]): Value[Vector[A]] =
//        copy(description = f(description))
//      override def modifyExample(f: Option[Vector[A]] => Option[Vector[A]]): Value[Vector[A]] =
//        copy(example = f(example))
//      override def decode(crock: OpenApi.Array[?]): Validated[Violations, Vector[A]] =
//        crock.toVector.zipWithIndex.traverse { case (crock, index) =>
//          schema.value.decode(crock).leftMap(_.modifyHistory(index /: _))
//        }
//      override def encode(a: Vector[A]): OpenApi.Array[OpenApi.Primitive] = OpenApi.Array(a.map(schema.value.encode))
//      override def parse(values: Vector[String]): Validated[Violations, Vector[A]] =
//        values.traverse(schema.value.parse)
//      override def render(a: Vector[A]): Vector[String] = a.map(schema.value.render)
//
//    final private case class Validate[A, B: Encoder, C](
//        collection: Collection.Value[A],
//        validation: Validation[B, A, A, C],
//        g: C => A
//    ) extends Collection.Value[C]:
//      export collection.{description, schema}
//      override def example: Option[C] = collection.example.flatMap(validation.run(_).toOption)
//      override def constraints: Chain[Constraint[OpenApi]] =
//        collection.constraints ++ validation.constraints.map(_.map(_.asOpenApi))
//      override def modifyDescription(f: Option[String] => Option[String]): Collection.Value[C] =
//        copy(collection = collection.modifyDescription(f))
//      override def modifyExample(f: Option[C] => Option[C]): Collection.Value[C] =
//        copy(collection = collection.modifyExample(a => f(a.flatMap(validation.run(_).toOption)).map(g)))
//      override def decode(crock: OpenApi.Array[?]): Validated[Violations, C] =
//        collection.decode(crock).andThen(applyValidation(validation, collection.encode))
//      override def encode(c: C): OpenApi.Array[OpenApi.Primitive] = collection.encode(g(c))
//      override def parse(values: Vector[String]): Validated[Violations, C] =
//        collection.parse(values).andThen(applyValidation(validation, collection.encode))
//      override def render(c: C): Vector[String] = collection.render(g(c))
//
//    def apply[A](schema: Eval[Schema.Value[A]]): Collection.Value[Vector[A]] = Root(none, none, schema)
