package io.taig.otter

/** Pairs two schemas into one that reads and writes both. Replaces the `Apply` / `ContravariantSemigroupal` /
  * `InvariantSemigroupal` triple that a direction split would otherwise require.
  */
trait Zip[F[- _, + _]]:
  def zip[W1, R1, W2, R2](left: F[W1, R1], right: F[W2, R2]): F[(W1, W2), (R1, R2)]

object Zip:
  inline def apply[F[- _, + _]](using zip: Zip[F]): Zip[F] = zip
