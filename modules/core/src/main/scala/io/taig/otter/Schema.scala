package io.taig.otter

import cats.data.Chain

sealed trait Schema[+F[_], A] extends Schema.Reader[F, A], Schema.Writer[F, A]:
  final def optional: Schema[F, Option[A]] = Schema.Optional(this)

object Schema:
  sealed trait Reader[+F[_], +A]:
    def optional: Schema.Reader[F, Option[A]]

  sealed trait Writer[+F[_], -A]:
    def optional: Schema.Writer[F, Option[A]]

  final case class Optional[F[_], A](self: Schema[F, A]) extends Schema[F, Option[A]]

  final case class Root[F[_], A](schema: F[A]) extends Schema[F, A]

// ---

sealed trait Codec[+S[+_], +A, B]

sealed trait Primitive[A] extends Codec[Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+S[+_], +A, B] extends Codec[S, A, B]

sealed trait Tuple[+S[+_], +A, B] extends Codec[S, A, B]

object Tuple:
  case object Empty extends Tuple[Nothing, Nothing, Unit]

  final case class One[S[+_], T[a] <: Schema[Codec[S, ?, *], a], A](schema: S[T[A]]) extends Tuple[S, S[T[A]], A]

final case class Annotation[+S, +M](self: S, metadata: M)

object Playground:
  val str: Primitive[String] = ???
  val strSchema: Schema[Primitive, String] = Schema.Root(str)
  val x: Schema[Primitive, Option[String]] = strSchema.optional
  val annotatedStr: Annotation[Schema[Primitive, String], Unit] = Annotation(strSchema, ())
  // val collectionSchema: Collection[?, Chain[String]] = Collection.Root(strSchema)
  // val annotatedCollection: Collection[Annotation[Schema[Primitive, String], Unit], Chain[String]] =
  //   Collection.Root[[a] =>> Annotation[Schema[Primitive, a], Unit], String](annotatedStr)
