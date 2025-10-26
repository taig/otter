package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordRenderer[-S[_], T](renderer: Renderer[S, (String, T)])
    extends Renderer[Record[S, *], Chain[(String, T)]]:
  override def render[A](schema: Record[S, A]): Chain[(String, T)] = schema match
    case Record.Empty              => Chain.empty
    case Record.Modify(self, _, _) => render(schema = self)
    case Record.Root(field)        => Chain.one(renderer.render(schema = field.value))
    case Record.Zip(left, right)   => render(schema = left) ++ render(schema = right)
