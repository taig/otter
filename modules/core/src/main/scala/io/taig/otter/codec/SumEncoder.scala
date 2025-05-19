package io.taig.otter.codec

import io.taig.otter.Discriminator
import io.taig.otter.Sum

final class SumEncoder[S[_], T](branch: Discriminator => Encoder[S, T]) extends Encoder[Sum[S, *], T]:
  override def encode[A](schema: Sum[S, A], a: A): T =
    encode(schema, discriminator = schema.discriminator, a)

  def encode[A](schema: Sum[S, A], discriminator: Discriminator, a: A): T = schema match
    case Sum.Modify(self, _, g) => encode(schema = self, discriminator, g(a))
    case Sum.OrElse(left, right, _, _) =>
      a.fold(encode(schema = left, discriminator, _), encode(schema = right, discriminator, _))
    case Sum.Root(branch, _, _) => this.branch(discriminator).encode(schema = branch.value, a)
