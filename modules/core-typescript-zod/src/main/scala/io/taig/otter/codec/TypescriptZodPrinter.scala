package io.taig.otter.codec

import io.taig.otter.Typescript
import cats.syntax.all.*
import io.taig.otter.zodObject
import cats.data.Chain
import scala.collection.immutable.ListMap
import cats.data.State
import scala.collection.immutable.SortedSet

object TypescriptZodPrinter:
  def print(typescript: Typescript): String = typescript match
    case Typescript.Any                => "z.any()"
    case Typescript.Array(self)        => s"z.array(${print(self)})"
    case Typescript.Boolean            => "z.boolean()"
    case Typescript.Literal(value)     => s"z.literal($value)"
    case Typescript.Nullable(self)     => s"z.nullable(${print(self)})"
    case Typescript.Number             => "z.number()"
    case Typescript.Object(fields)     => zodObject(fields.map(_.map(print(_))))
    case Typescript.Record(key, value) => s"z.record(${print(key)}, ${print(value)})"
    case Typescript.Reference(name)    => name
    case Typescript.String             => s"z.string()"
    case Typescript.Tuple(values)      => s"z.tuple([${values.map(print).mkString_(", ")}])"
    case typescript: Typescript.Union  => print(typescript).map(print).mkString_("z.union([", ", ", "])")
    case Typescript.Void               => s"z.void()"

  def print(typescript: Typescript.Union): Chain[Typescript] = typescript match
    case Typescript.Union(left: Typescript.Union, right: Typescript.Union) => print(left) ++ print(right)
    case Typescript.Union(left: Typescript.Union, right)                   => print(left) :+ right
    case Typescript.Union(left, right: Typescript.Union)                   => left +: print(right)
    case Typescript.Union(left, right)                                     => Chain(left, right)

object TypescriptZodPrinter2:
  def print(
      references: ListMap[String, Typescript],
      typescript: Typescript,
      stack: SortedSet[String] = SortedSet.empty
  ): State[ListMap[String, (Typescript, String)], (Typescript, String)] =
    typescript match
      case Typescript.Any => State.pure((typescript, "z.any()"))
      case Typescript.Nullable(self) =>
        print(references, self, stack).map(expression => s"z.nullable(${expression._2})").tupleLeft(typescript)
      case Typescript.Reference(name) =>
        if stack.contains(name)
        then State.pure((typescript, s"z.lazy(() => $name)"))
        else
          print(references, typescript = references(name), stack + name).transform((state, value) =>
            (state + (name -> value), (typescript, name))
          )
      case Typescript.Union(left, right) =>
        (print(references, left, stack), print(references, right, stack))
          .mapN((left, right) => s"z.union([${left._2}, ${right._2}])")
          .tupleLeft(typescript)
      case Typescript.Number  => State.pure((typescript, "z.number()"))
      case Typescript.Boolean => State.pure((typescript, "z.boolean()"))
      case Typescript.String  => State.pure((typescript, "z.string()"))
      case Typescript.Object(fields) =>
        fields
          .traverse((name, typescript) => print(references, typescript, stack).tupleLeft(name))
          .map(_.map { case (a, (_, b)) => (a, b) })
          .map(zodObject)
          .tupleLeft(typescript)
      case Typescript.Array(self) =>
        print(references, self, stack).map(expression => s"z.array(${expression._2})").tupleLeft(typescript)
      case Typescript.Record(key, value) =>
        (print(references, key, stack), print(references, value, stack))
          .mapN((key, value) => s"z.record(${key._2}, ${value._2})")
          .tupleLeft(typescript)
