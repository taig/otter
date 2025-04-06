package io.taig.otter

object JsonKeyReferenceConstantPrinter:
  def apply[A](reference: Reference.Constant[Json.Key, A]): String =
    JsonKeyPrinter(codec = reference.self.value, reference.value)
