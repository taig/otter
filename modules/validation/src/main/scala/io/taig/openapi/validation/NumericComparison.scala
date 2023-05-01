package io.taig.openapi.validation

final case class NumericComparison[A](reference: A, equal: Boolean, delta: Option[A]):
  def modifyEqual(f: Boolean => Boolean): NumericComparison[A] = copy(equal = f(equal))
  def withEqual(equal: Boolean): NumericComparison[A] = modifyEqual(_ => equal)

object NumericComparison:
  def equal[A](reference: A): NumericComparison[A] = NumericComparison(reference, equal = true, delta = None)
