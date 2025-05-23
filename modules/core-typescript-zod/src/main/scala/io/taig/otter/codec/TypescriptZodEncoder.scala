package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.zodObject
import cats.data.NonEmptyChain
import cats.data.Chain
import scala.collection.immutable.ListMap
import cats.data.State
import io.taig.otter.TypescriptZodState
import io.taig.otter.zodUnion
import io.taig.otter.TypescriptZod

object TypescriptZodEncoder:
  def encode(references: ListMap[String, Typescript], typescript: Typescript): TypescriptZodState[String] =
    apply(references, typescript).map(_.expression)

  private def apply(references: ListMap[String, Typescript], typescript: Typescript): TypescriptZodState[TypescriptZod] =
    typescript match
      case Typescript.Any => State.pure(TypescriptZod(typescript, "z.any()"))
      case Typescript.Array(self) =>
        apply(references, self).map(zod => TypescriptZod(typescript, expression = s"z.array(${zod.expression})"))
      case Typescript.Boolean        => State.pure(TypescriptZod(typescript, expression = "z.boolean()"))
      case Typescript.Literal(value) => State.pure(TypescriptZod(typescript, s"z.literal($value)"))
      case Typescript.Nullable(self) =>
        apply(references, self).map(zod => TypescriptZod(typescript, expression = s"z.nullable(${zod.expression})"))
      case Typescript.Number => State.pure(TypescriptZod(typescript, expression = "z.number()"))
      case Typescript.Object(fields) =>
        fields
          .traverse((name, typescript) => apply(references, typescript).tupleLeft(name))
          .map(_.map { case (name, zod) => (name, zod.expression) })
          .map(zodObject)
          .map(TypescriptZod(typescript, _))
      case Typescript.Record(key, value) =>
        (apply(references, key), apply(references, value)).mapN: (key, value) =>
          TypescriptZod(typescript, expression = s"z.record(${key.expression}, ${value.expression})")
      case Typescript.Reference(name) =>
        State: context =>
          if context.stack.contains_(name) then
            (context, TypescriptZod(typescript, expression = s"z.lazy(() => $name)"))
          else if context.references.contains(name) then (context, TypescriptZod(typescript, expression = name))
          else
            val (update, zod) = apply(references, typescript = references(name)).run(initial = context.push(name)).value
            (
              update.pop.modifyReferences(_.updatedWith(name)(_ => zod.some)),
              TypescriptZod(typescript = typescript, expression = name)
            )
      case Typescript.String => State.pure(TypescriptZod(typescript, expression = "z.string()"))
      case Typescript.Tuple(values) =>
        values
          .traverse(apply(references, _).map(_.expression))
          .map: values =>
            TypescriptZod(typescript, expression = s"z.tuple([${values.mkString_(", ")}])")
      case typescript: Typescript.Union =>
        apply(references, typescript).flatMap: values =>
          values
            .traverse(apply(references, _).map(_.expression))
            .map(values => TypescriptZod(typescript, expression = zodUnion(values)))
      case Typescript.Void => State.pure(TypescriptZod(typescript, expression = "z.void()"))

  private def apply(
      references: ListMap[String, Typescript],
      typescript: Typescript.Union
  ): TypescriptZodState[NonEmptyChain[Typescript]] = typescript match
    case Typescript.Union(left: Typescript.Union, right: Typescript.Union) =>
      (apply(references, left), apply(references, right)).mapN(_ ++ _)
    case Typescript.Union(left: Typescript.Union, right) => apply(references, left).map(_ :+ right)
    case Typescript.Union(left, right: Typescript.Union) => apply(references, right).map(left +: _)
    case Typescript.Union(left, right)                   => State.pure(NonEmptyChain(left, right))
