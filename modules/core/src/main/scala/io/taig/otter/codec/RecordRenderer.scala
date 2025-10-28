package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record

final class RecordRenderer[-S[_], F[_]: Applicative, T](renderer: Renderer[S, F[(String, T)]])
    extends Renderer[Record[S, *], F[Chain[(String, T)]]]:
  override def render[A](schema: Record[S, A]): F[Chain[(String, T)]] = schema match
    case Record.Empty              => Chain.empty.pure
    case Record.Modify(self, _, _) => render(schema = self)
    case Record.Root(field)        => renderer.render(schema = field.value).map(Chain.one)
    case Record.Zip(left, right)   => (render(schema = left), render(schema = right)).mapN(_ ++ _)
