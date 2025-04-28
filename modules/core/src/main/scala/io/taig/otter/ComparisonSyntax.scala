package io.taig.otter

trait ComparisonSyntax:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

object ComparisonSyntax extends ComparisonSyntax
