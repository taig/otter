package io.taig.otter

import cats.Contravariant
import cats.Invariant
import cats.Functor
import io.taig.otter.validation.Validation
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait SchemaInvariant[F[_], G[_]] extends Invariant[F]:
  extension [A](self: F[A])
    def constraints: Chain[Constraint[?]]
    def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): F[B]
    def validate_[V1, V2](validation: SchemaValidation[A, V1, V2, Unit]): F[A] = ivalidate(validation.tap)(identity)
    def optional: G[Option[A]]

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = ivalidate(fa)(Validation.lift(f))(g)

trait SchemaFunctor[F[_], G[_]] extends SchemaInvariant[F, G], Functor[F]:
  extension [A](self: F[A])
    def validate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B]): F[B]
    override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): F[B] =
      validate(validation)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))

trait SchemaContravariant[F[_], G[_]] extends SchemaInvariant[F, G], Contravariant[F]:
  extension [A](self: F[A]) final override def constraints: Chain[Constraint[?]] = Chain.empty

trait PrimitiveInvariant[F[_], G[_]] extends SchemaInvariant[F, G]:
  extension [A](self: F[A]) def tpe: Type[?]
