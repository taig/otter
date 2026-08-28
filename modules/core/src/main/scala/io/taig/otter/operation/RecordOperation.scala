package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Reference

/** Constructs the record type `F` from fields of type `G`. */
trait RecordOperation[F[- _, + _], G[- _, + _]]:
  def empty: F[Unit, Unit]

  def lift[W, R](field: Reference[G, W, R]): F[W, R]

  extension [W, R](fa: F[W, R]) def fields: Chain[Reference[G, ?, ?]]

object RecordOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: RecordOperation[F, G]): RecordOperation[F, G] = self

/** Lets a single field stand in for the record that holds it, so that a chain can start with `field(...)`. */
trait RecordableOperation[F[- _, + _], G[- _, + _]]:
  extension [W, R](fa: F[W, R]) def toRecord: G[W, R]

object RecordableOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: RecordableOperation[F, G]): RecordableOperation[F, G] = self

  def derived[F[- _, + _], G[- _, + _]](using G: RecordOperation[G, F]): RecordableOperation[F, G] =
    new RecordableOperation[F, G]:
      extension [W, R](fa: F[W, R]) override def toRecord: G[W, R] = G.lift(Reference.now(fa))

  /** A record is already a record. Lets one `:*` serve both `field :* field` and `record :* field`. */
  def identity[F[- _, + _]]: RecordableOperation[F, F] = new RecordableOperation[F, F]:
    extension [W, R](fa: F[W, R]) override def toRecord: F[W, R] = fa
