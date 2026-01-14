package io.taig.otter.component

import io.taig.otter.operation.EnumerationOperation
import io.taig.otter.Reference
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues
import cats.Order
import scala.annotation.targetName

trait EnumerationComponent[F[_], G[_]](using F: EnumerationOperation[F, G]):
  def enumeration[A, B](schema: => G[A], mapping: Mapping[B, A]): F[B] =
    F.lift(schema = Reference.later(schema), mapping)

  def enumeration[A: Order, B](schema: => G[A])(f: B => A)(using EnumerationValues.Aux[B, B]): F[B] =
    enumeration(schema, mapping = Mapping.enumeration(f))

object EnumerationComponent:
  trait Read[F[_], G[_]](using F: EnumerationOperation.Read[F, G]):
    @targetName("enumerationRead")
    def enumeration[A, B](schema: => G[A], mapping: Mapping[B, A]): F[B] =
      F.lift(schema = Reference.later(schema), mapping)

    @targetName("enumerationRead")
    def enumeration[A: Order, B](schema: => G[A])(f: B => A)(using EnumerationValues.Aux[B, B]): F[B] =
      enumeration(schema, mapping = Mapping.enumeration(f))

  trait Write[F[_], G[_]](using F: EnumerationOperation.Write[F, G]):
    @targetName("enumerationWrite")
    def enumeration[A, B](schema: => G[A], mapping: Mapping[B, A]): F[B] =
      F.lift(schema = Reference.later(schema), mapping)

    @targetName("enumerationWrite")
    def enumeration[A: Order, B](schema: => G[A])(f: B => A)(using EnumerationValues.Aux[B, B]): F[B] =
      enumeration(schema, mapping = Mapping.enumeration(f))
