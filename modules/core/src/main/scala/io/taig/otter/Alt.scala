package io.taig.otter

/** Combines two schemas into one that reads and writes either of them. */
trait Alt[F[-_, +_]]:
  def alt[W1, R1, W2, R2](left: F[W1, R1], right: F[W2, R2]): F[Either[W1, W2], Either[R1, R2]]

object Alt:
  inline def apply[F[-_, +_]](using alt: Alt[F]): Alt[F] = alt
