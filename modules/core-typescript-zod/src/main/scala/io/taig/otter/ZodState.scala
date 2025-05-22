package io.taig.otter

import cats.data.State

import scala.collection.immutable.ListMap

type ZodState[A] = State[ZodState.Context, A]

object ZodState:
  final case class Context(references: ListMap[String, Zod[String]], stack: List[String]):
    def modifyStack(f: List[String] => List[String]): ZodState.Context =
      copy(stack = f(stack))

    def push(stack: String): ZodState.Context = modifyStack(stack :: _)

    def pop: ZodState.Context = modifyStack(_.tail)

    def modifyReferences(f: ListMap[String, Zod[String]] => ListMap[String, Zod[String]]): ZodState.Context =
      copy(references = f(references))

  object Context:
    val Empty: ZodState.Context = Context(references = ListMap.empty, stack = Nil)
