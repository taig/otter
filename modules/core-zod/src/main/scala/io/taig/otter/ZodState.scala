package io.taig.otter

import cats.data.State

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet

type ZodState[A] = State[ZodState.Context, A]

object ZodState:
  final case class Context(references: ListMap[String, Zod], stack: SortedSet[String]):
    def modifyReferences(f: ListMap[String, Zod] => ListMap[String, Zod]): ZodState.Context =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): ZodState.Context = copy(stack = f(stack))

    def push(stack: String): ZodState.Context = modfyStack(_ + stack)

    def pop(stack: String): ZodState.Context = modfyStack(_ - stack)

  object Context:
    val Empty: ZodState.Context = Context(references = ListMap.empty, stack = SortedSet.empty)
