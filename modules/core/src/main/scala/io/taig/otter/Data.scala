package io.taig.otter

sealed trait Data[+S[+_], +A <: S[Schema.Any[S, ?]], B]

sealed trait Primitive[A] extends Data[Nothing, Nothing, A]

object Primitive:
  final case class Root[A](tpe: Type[A]) extends Primitive[A]

sealed trait Tuple[+S[+_], +A <: S[Schema.Any[S, ?]], B] extends Data[S, A, B]

object Tuple:
  case object Empty extends Tuple[Nothing, Nothing, Unit]
  final case class One[+S[+_], T[a] <: Schema.Any[S, a], A](data: S[T[A]]) extends Tuple[S, S[T[A]], A]
