package io.taig.openapi.schema

import cats.{Invariant, InvariantMonoidal, Semigroupal}
import io.taig.openapi.Encoder
import io.taig.screening.{Validation, Violation}

import scala.{deriving, Product as SProduct}
import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror.fromProductTyped

trait InvariantValidation[F[_]] extends Invariant[F]:
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  def ivalidate[A: Encoder, B, C](fa: F[B])(validation: Validation[A, B, B, C])(g: C => B): F[C]

  extension [F[_], A](fa: F[A])(using F: InvariantValidation[F])
    def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): F[C] = F.ivalidate(fa)(validation)(g)
    def validate[B: Encoder](validation: Validation[B, A, A, Unit]): F[A] = ivalidate(validation.tap)(identity)

object InvariantValidation:
  trait Product[F[_]] extends InvariantValidation[F] with InvariantMonoidal[F]:
    extension [F[_], A](fa: F[A])(using F: InvariantValidation[F])
      def gimap[B](using evidence: Evidence.Product.Aux[B, A]): F[B] = F.imap(fa)(evidence.from)(evidence.to)

  trait Sum[F[_]] extends InvariantValidation[F]:
    extension [F[_], A](fa: F[A])(using F: InvariantValidation[F])
      def gimap[B](using evidence: Evidence.Sum.Aux[B, A]): F[B] = F.imap(fa)(evidence.from)(evidence.to)
