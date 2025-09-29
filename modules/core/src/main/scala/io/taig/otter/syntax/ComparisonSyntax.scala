package io.taig.otter.syntax

import io.taig.otter.validation.Comparison

trait ComparisonSyntax:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

object ComparisonSyntax extends ComparisonSyntax
