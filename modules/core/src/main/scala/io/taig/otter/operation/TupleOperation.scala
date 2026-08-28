package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Reference

/** Constructs the tuple type `F` over element schemas of type `G`. */
trait TupleOperation[F[-_, +_], G[-_, +_]]:
  def empty: F[Unit, Unit]

  def lift[W, R](schema: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R]) def schemas: Chain[Reference[G, ?, ?]]

object TupleOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: TupleOperation[F, G]): TupleOperation[F, G] = self

/** Lets a single schema stand in for the tuple that holds it. */
trait TupleableOperation[F[-_, +_], G[-_, +_]]:
  extension [W, R](fa: F[W, R]) def toTuple: G[W, R]

object TupleableOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: TupleableOperation[F, G]): TupleableOperation[F, G] = self

  def derived[F[-_, +_], G[-_, +_]](using G: TupleOperation[G, F]): TupleableOperation[F, G] =
    new TupleableOperation[F, G]:
      extension [W, R](fa: F[W, R]) override def toTuple: G[W, R] = G.lift(Reference.now(fa))

  /** A tuple is already a tuple. Lets one `:*` serve both `TNil :* string` and `tuple :* string`. */
  def identity[F[-_, +_]]: TupleableOperation[F, F] = new TupleableOperation[F, F]:
    extension [W, R](fa: F[W, R]) override def toTuple: F[W, R] = fa
