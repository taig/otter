package io.taig.otter

import cats.syntax.all.*

final class EnumerationZodRenderer[S[_]](printer: Printer[S]) extends Renderer[Enumeration[S, *], String]:
  override def apply[T](codec: Enumeration[S, T]): String = codec match
    case Enumeration.Modify(self, _, _) => apply(codec = self)
    case codec @ Enumeration.Root(reference, mapping, _) =>
      val values = codec.values.map(mapping.apply).map(a => printer(codec = reference.value, a))
      s"z.enum([${values.mkString_(", ")}])"
