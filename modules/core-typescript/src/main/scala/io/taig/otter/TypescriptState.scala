package io.taig.otter

import scala.collection.immutable.ListMap
import cats.data.State
import scala.collection.immutable.SortedSet

type TypescriptState[A] = State[TypescriptState.Context, A]

object TypescriptState:
  final case class Context(references: ListMap[String, Typescript], stack: SortedSet[String]):
    def modifyReferences(f: ListMap[String, Typescript] => ListMap[String, Typescript]): TypescriptState.Context =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): TypescriptState.Context = copy(stack = f(stack))

    def push(stack: String): TypescriptState.Context = modfyStack(_ + stack)

    def pop(stack: String): TypescriptState.Context = modfyStack(_ - stack)

    def definitions: List[TypescriptDefinition[?]] = references.toList.map(TypescriptDefinition.apply)

  object Context:
    val Empty: TypescriptState.Context = Context(references = ListMap.empty, stack = SortedSet.empty)
