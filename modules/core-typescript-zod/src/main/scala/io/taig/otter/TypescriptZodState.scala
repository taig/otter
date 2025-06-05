package io.taig.otter

import cats.data.State

import io.taig.otter.Typescript
import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet

type TypescriptZodState[A] = State[TypescriptZodState.Context, A]

object TypescriptZodState:
  enum Reference:
    case Shared(self: Typescript.Value)
    case Type(self: Typescript.Value)
    case Expression(self: Typescript.Value)

  final case class Context(references: ListMap[String, Reference], stack: SortedSet[String]):
    def modifyReferences(f: ListMap[String, Reference] => ListMap[String, Reference]): TypescriptZodState.Context =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): TypescriptZodState.Context = copy(stack = f(stack))

    def push(stack: String): TypescriptZodState.Context = modfyStack(_ + stack)

    def pop(stack: String): TypescriptZodState.Context = modfyStack(_ - stack)

  object Context:
    val Empty: TypescriptZodState.Context = Context(references = ListMap.empty, stack = SortedSet.empty)
