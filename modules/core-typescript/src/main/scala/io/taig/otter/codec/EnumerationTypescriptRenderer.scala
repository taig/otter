package io.taig.otter.codec
import cats.data.NonEmptyList
import io.taig.otter.Enumeration
import io.taig.otter.Typescript

final class EnumerationTypescriptRenderer[S[_]](printer: Encoder[S, String])
    extends Renderer[Enumeration[S, *], Typescript]:
  override def render[A](schema: Enumeration[S, A]): Typescript = render(schema = schema.value)

  def render[A](schema: Enumeration.Value[S, A]): Typescript = schema match
    case Enumeration.Value.Modify(self, _, _) => render(schema = self)
    case schema @ Enumeration.Value.Root(reference, mapping) =>
      val NonEmptyList(left, tail) = schema.values
        .map(mapping.apply)
        .map(a => printer.encode(schema = reference.value, a))
        .map(Typescript.Literal.apply)

      tail match
        case right :: tail => tail.foldLeft(Typescript.Union(left, right))(Typescript.Union.apply)
        case Nil           => left
