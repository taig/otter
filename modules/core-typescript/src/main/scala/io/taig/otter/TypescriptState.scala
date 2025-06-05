package io.taig.otter

import cats.data.State

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet
import cats.Functor
import cats.syntax.all.*
import cats.derived.*
import cats.Applicative

type TypescriptState[A, B] = State[TypescriptState.Context[A], B]

object TypescriptState:
  final case class Context[+A](references: ListMap[String, A], stack: SortedSet[String]):
    def map[B](f: A => B): TypescriptState.Context[B] = copy(references = references.view.mapValues(f).to(ListMap))

    def modifyReferences[A1 >: A](f: ListMap[String, A] => ListMap[String, A1]): TypescriptState.Context[A1] =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): TypescriptState.Context[A] = copy(stack = f(stack))

    def push(stack: String): TypescriptState.Context[A] = modfyStack(_ + stack)

    def pop(stack: String): TypescriptState.Context[A] = modfyStack(_ - stack)

  object Context:
    val Empty: TypescriptState.Context[Nothing] = Context(references = ListMap.empty, stack = SortedSet.empty)

    given Functor[TypescriptState.Context] with
      override def map[A, B](fa: Context[A])(f: A => B): Context[B] = fa.map(f)
