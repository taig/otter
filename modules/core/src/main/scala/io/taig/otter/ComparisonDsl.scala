package io.taig.otter

trait ComparisonDsl:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

object ComparisonDsl extends ComparisonDsl
