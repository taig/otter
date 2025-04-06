package io.taig.otter

final class ReferenceConstantZodEncoder[S[_], T](encoder: Encoder[S, T]):
  def apply[A](codec: Reference.Constant[S, A]): T = encoder(codec.self.value, codec.value)
