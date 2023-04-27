package io.taig.openapi.schema

import scala.deriving.Mirror

abstract class EnumValues[A]:
  type Out
  def toSet: Set[Out]

object EnumValues:
  type Aux[A, B] = EnumValues[A] { type Out = B }

  inline given [A](using
      mirror: Mirror.SumOf[A],
      values: EnumValues.Aux[mirror.MirroredElemTypes, A]
  ): EnumValues.Aux[A, A] = new EnumValues[A]:
    override type Out = A
    override val toSet: Set[A] = values.toSet

  inline given singleton[A <: Singleton, B <: Tuple, C >: A](using
      values: EnumValues.Aux[B, C]
  ): EnumValues.Aux[A *: B, C] = new EnumValues[A *: B]:
    override type Out = C
    override def toSet: Set[C] = Set(valueOf[A]) ++ values.toSet

  inline given nested[A, B <: Tuple, C >: A](using
      mirror: Mirror.SumOf[A],
      head: EnumValues.Aux[mirror.MirroredElemTypes, C],
      tail: EnumValues.Aux[B, C]
  ): EnumValues.Aux[A *: B, C] = new EnumValues[A *: B]:
    override type Out = C
    override def toSet: Set[C] = head.toSet ++ tail.toSet

  inline given [A]: EnumValues.Aux[EmptyTuple, A] = new EnumValues[EmptyTuple]:
    override type Out = A
    override def toSet: Set[A] = Set.empty
