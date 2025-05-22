package io.taig.otter

import cats.data.State

import scala.collection.immutable.SortedSet
import scala.collection.immutable.ListMap

type ZodState[A] = State[ZodState.Context, A]

object ZodState:
  final case class Context(stack: SortedSet[String], references: ListMap[ZodConst, String], recursion: Boolean):
    def modifyStack(f: SortedSet[String] => SortedSet[String]): ZodState.Context =
      copy(stack = f(stack))

    def put(stack: String): ZodState.Context = modifyStack(_ + stack)

    def remove(stack: String): ZodState.Context = modifyStack(_ - stack)

    def modifyReferences(f: ListMap[ZodConst, String] => ListMap[ZodConst, String]): ZodState.Context =
      copy(references = f(references))

  object Context:
    val Empty: ZodState.Context = Context(stack = SortedSet.empty, references = ListMap.empty, recursion = false)
