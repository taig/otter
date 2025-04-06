package io.taig.otter

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping

trait EnumerationInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B]

  extension [A](self: Self[A]) def values: NonEmptyList[A]

object EnumerationInvariant:
  trait Lift[Self[_], Value[_]] extends EnumerationInvariant[Self, Value]:
    def lift[A](codec: Enumeration[Value, A]): Self[A]

    final override def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B] =
      lift(Enumeration.Root(codec = Reference.later(codec), mapping, metadata = Metadata.Empty))
