package io.taig.otter

import cats.Eq
import cats.Functor
import cats.derived.*

final case class Comparison[A](reference: A, exclusive: Boolean) derives Eq, Functor:
  def map[B](f: A => B): Comparison[B] = copy(reference = f(reference))

object Comparison:
  trait Syntax:
    def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

  object Syntax extends Syntax

//   trait Component[Nullable[a] <: Value[a], Record[_], Field[_], Key[_], Value[_]](using
//       Codec.Field[Field, Key, Value, Record],
//       Codec.Record[Record, Field]
//   ) extends NullableDsl[Nullable, Value], PrimitiveDsl.Boolean[Value],
//         FieldDsl.Primitive.String[Field, Key, Value, Record]:
//     def comparison[A](codec: => Value[A]): Record[Comparison[A]] =
//       (field("reference", codec) :* field("exclusive", nullable(boolean, default = false))).to
