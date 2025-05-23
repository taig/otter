package io.taig.otter

import cats.data.State

import scala.collection.immutable.ListMap

type TypescriptZodState[A] = State[TypescriptZodState.Context, A]

object TypescriptZodState:
  final case class Context(references: ListMap[String, TypescriptZod], stack: List[String]):
    def modifyStack(f: List[String] => List[String]): TypescriptZodState.Context =
      copy(stack = f(stack))

    def push(stack: String): TypescriptZodState.Context = modifyStack(stack :: _)

    def pop: TypescriptZodState.Context = modifyStack(_.tail)

    def modifyReferences(f: ListMap[String, TypescriptZod] => ListMap[String, TypescriptZod]): TypescriptZodState.Context =
      copy(references = f(references))

    def definitions: List[TypescriptZodDefinition] = references.toList.map((name, zod) => zod.definition(name))

  object Context:
    val Empty: TypescriptZodState.Context = Context(references = ListMap.empty, stack = Nil)
