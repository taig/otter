package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import io.taig.otter.Enumeration
import io.taig.otter.Zod

final class EnumerationZodRenderer[S[_]](printer: Encoder[S, String]) extends Renderer[Enumeration[S, *], Zod]:
  override def render[A](schema: Enumeration[S, A]): Zod = render(schema = schema.value)

  def render[A](schema: Enumeration.Value[S, A]): Zod = schema match
    case Enumeration.Value.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Value.Root(reference, mapping) =>
      val NonEmptyList(left, tail) = schema.values
        .map(mapping.apply)
        .map(printer.encode(schema = reference.value, _))
        .map(Zod.Literal.apply)

      tail match
        case Nil => left
        case _   => Zod.Union(NonEmptyChain.fromChainPrepend(left, Chain.fromSeq(tail)))
