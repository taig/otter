package io.taig.otter.codec

import io.taig.otter.Sum
import io.taig.otter.Discriminator
import cats.data.Chain

final class SumEncoder[S[_], T, U](branch: Discriminator => Encoder[S, Chain[(T, U)]])
    extends Encoder[Sum[S, *], Chain[(T, U)]]:
  override def encode[A](schema: Sum[S, A], a: A): Chain[(T, U)] =
    encode(schema, discriminator = schema.discriminator, a)

  def encode[A](schema: Sum[S, A], discriminator: Discriminator, a: A): Chain[(T, U)] = schema match
    case Sum.Modify(self, _, g) => encode(schema = self, discriminator, g(a))
    case Sum.OrElse(left, right, _, _) =>
      a.fold(encode(schema = left, discriminator, _), encode(schema = right, discriminator, _))
    case Sum.Root(branch, _, _) => this.branch(discriminator).encode(schema = branch.value, a)
