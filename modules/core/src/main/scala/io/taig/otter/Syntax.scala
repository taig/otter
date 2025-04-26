package io.taig.otter

trait Syntax:
  def comparison[A](reference: A, exclusive: Boolean = false): Comparison[A] = Comparison(reference, exclusive)

object Syntax extends Syntax
