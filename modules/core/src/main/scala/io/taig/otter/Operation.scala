package io.taig.otter

import io.taig.otter.validation.Validation

trait Operation[+Self[a] <: Schema[a], +Optional[_], Schema[_], Tuple[_], A]:
  def asSelf: Self[A]

  def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(g: C => A): Self[C]
  final def ivalidate[B](validation: Validation[A, A, B])(g: B => A): Self[B] = ivalidate(asSelf)(validation)(g)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)
  def optional: Optional[Option[A]]
  def toTuple: Tuple[A]

object Operation:
  trait Value[+Self[a] <: Schema[a], +Optional[_], Schema[_], Tuple[_], A]
      extends Operation[Self, Optional, Schema, Tuple, A]

  trait Primitive[+Self[a] <: Schema[a], +Optional[_], Schema[_], Tuple[_], A]
      extends Value[Self, Optional, Schema, Tuple, A]:
    def tpe: Type[?]

  trait Tuple[Self[a] <: Schema[a], Schema[_], A] extends Operation[Self, Self, Schema, Self, A]:
    def size: Int
    def product[B](tuple: Self[B]): Self[(A, B)]
