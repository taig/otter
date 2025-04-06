package io.taig.otter

import cats.syntax.all.*

final class UnionZodRenderer[S[_]](render: [A] => (String, S[A]) => ZodState[Expression])
    extends Renderer[Union[S, *], ZodState[String]]:
  override def apply[A](codec: Union[S, A]): ZodState[String] = codec.branches
    .traverse((name, reference) => render(name, reference.value))
    .map: values =>
      s"""z.union([
         |${indent(values.map(value => show"$value").mkString_(",\n"))}
         |])""".stripMargin
