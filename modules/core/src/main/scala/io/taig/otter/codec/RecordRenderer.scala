package io.taig.otter.codec

import cats.Applicative
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Record

final class RecordRenderer[-S[_], F[_]: Applicative, T](renderer: Renderer[S, F[T]])
    extends Renderer[Record[S, *], F[Chain[(String, T)]]]:
  override def render[A](schema: Record[S, A]): F[Chain[(String, T)]] = schema match
    case Record.Default(self, _)   => render(schema = self)
    case Record.Empty              => Chain.empty.pure
    case Record.Modify(self, _, _) => render(schema = self)
    case Record.Optional(self)     => render(schema = self)
    case Record.Root(field)        => renderer.render(schema = field.schema.value).tupleLeft(field.name).map(Chain.one)
    case Record.Zip(left, right)   => (render(schema = left), render(schema = right)).mapN(_ ++ _)

object RecordRenderer:
  def apply[S[_], F[_]: Applicative, T](renderer: Renderer[S, F[T]]): Renderer[Record[S, *], F[Chain[(String, T)]]] =
    new RecordRenderer(renderer)
