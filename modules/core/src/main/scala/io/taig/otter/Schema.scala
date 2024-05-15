package io.taig.otter

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

sealed trait Codec[+A, B]

sealed trait Primitive[A] extends Codec[Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Collection[+A, B] extends Codec[A, B]

sealed trait Tuple[+A, B] extends Codec[A, B]

object Tuple:
  case object Empty extends Tuple[Nothing, Unit]

  final case class One[+S[+_[+_]], +C[+_, _], A](schema: S[[a] =>> Schema[C[a, *], A]])
      extends Tuple[S[[a] =>> Schema[[x] =>> C[a, x], A]], A]
