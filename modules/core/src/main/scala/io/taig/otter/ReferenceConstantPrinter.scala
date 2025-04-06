package io.taig.otter

final class ReferenceConstantPrinter[S[_]](printer: Printer[S]):
  def apply[A](reference: Reference.Constant[S, A]): String = printer(reference.self.value, reference.value)
