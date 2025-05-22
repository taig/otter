package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.Zod
import io.taig.otter.zodObject
import cats.data.NonEmptyChain
import cats.data.Chain
import scala.collection.immutable.ListMap
import cats.data.State
import io.taig.otter.ZodState
import io.taig.otter.zodUnion

object TypescriptZodPrinter:
  def print(references: ListMap[String, Typescript], typescript: Typescript): ZodState[Zod[String]] = typescript match
    case Typescript.Any => State.pure(Zod(typescript, "z.any()"))
    case Typescript.Array(self) =>
      print(references, self).map(zod => Zod(typescript, expression = s"z.array(${zod.expression})"))
    case Typescript.Boolean        => State.pure(Zod(typescript, expression = "z.boolean()"))
    case Typescript.Literal(value) => State.pure(Zod(typescript, s"z.literal($value)"))
    case Typescript.Nullable(self) =>
      print(references, self).map(_.map(expression => s"z.nullable($expression)"))
    case Typescript.Number => State.pure(Zod(typescript, expression = "z.number()"))
    case Typescript.Object(fields) =>
      fields
        .traverse((name, typescript) => print(references, typescript).tupleLeft(name))
        .map(_.map { case (name, zod) => (name, zod.expression) })
        .map(zodObject)
        .map(Zod(typescript, _))
    case Typescript.Record(key, value) =>
      (print(references, key), print(references, value)).mapN: (key, value) =>
        Zod(typescript, expression = s"z.record(${key.expression}, ${value.expression})")
    case Typescript.Reference(name) =>
      State: context =>
        if context.stack.contains_(name) then (context, Zod(typescript, expression = s"z.lazy(() => $name)"))
        else if context.references.contains(name) then
          (context, Zod(typescript, expression = name))
        else
          val (update, zod) = print(references, typescript = references(name)).run(initial = context.push(name)).value
          (update.pop.modifyReferences(_.updatedWith(name)(_ => zod.some)), Zod(typescript, expression = name))
    case Typescript.String        => State.pure(Zod(typescript, expression = "z.string()"))
    case Typescript.Tuple(values) =>
      values.traverse(print(references, _).map(_.expression)).map: values =>
        Zod(typescript, expression = s"z.tuple([${values.mkString_(", ")}])")
    case typescript: Typescript.Union =>
      print(references, typescript).flatMap: values =>
        values
          .traverse(print(references, _).map(_.expression))
          .map(values => Zod(typescript, expression = zodUnion(values)))
    case Typescript.Void => State.pure(Zod(typescript, expression = "z.void()"))

  def print(references: ListMap[String, Typescript], typescript: Typescript.Union): ZodState[NonEmptyChain[Typescript]] =
    typescript match
      case Typescript.Union(left: Typescript.Union, right: Typescript.Union) =>
        (print(references, left), print(references, right)).mapN(_ ++ _)
      case Typescript.Union(left: Typescript.Union, right) => print(references, left).map(_ :+ right)
      case Typescript.Union(left, right: Typescript.Union) => print(references, right).map(left +: _)
      case Typescript.Union(left, right)                   => State.pure(NonEmptyChain(left, right))
