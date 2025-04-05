package io.taig.otter

final class ReferenceConstantPrinter[S[_]](printer: Printer[S]):
  def apply[A](codec: Reference.Constant[S, A]): String = printer(codec.self.value, codec.value)
