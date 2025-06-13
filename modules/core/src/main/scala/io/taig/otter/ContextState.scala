package io.taig.otter

import cats.data.State

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet
import cats.Functor
import cats.syntax.all.*
import cats.FunctorFilter

type ContextState[A, B] = State[ContextState.Context[A], B]

object ContextState:
  final case class Context[+A](references: ListMap[String, A], stack: SortedSet[String], recursion: SortedSet[String]):
    def map[B](f: A => B): ContextState.Context[B] = copy(references = references.view.mapValues(f).to(ListMap))

    def modifyReferences[A1 >: A](f: ListMap[String, A] => ListMap[String, A1]): ContextState.Context[A1] =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): ContextState.Context[A] = copy(stack = f(stack))

    def push(stack: String): ContextState.Context[A] = modfyStack(_ + stack)

    def pop(stack: String): ContextState.Context[A] = modfyStack(_ - stack).modifyRecursion(_ - stack)

    def modifyRecursion(f: SortedSet[String] => SortedSet[String]): ContextState.Context[A] =
      copy(recursion = f(recursion))

    def recurse(name: String): ContextState.Context[A] = modifyRecursion(_ + name)

  object Context:
    val Empty: ContextState.Context[Nothing] = Context(references = ListMap.empty, stack = SortedSet.empty, recursion = SortedSet.empty)

    given functor: Functor[ContextState.Context] with
      override def map[A, B](fa: Context[A])(f: A => B): Context[B] = fa.map(f)

    given FunctorFilter[ContextState.Context] with
      override def functor: Functor[ContextState.Context] = Context.functor

      override def mapFilter[A, B](fa: Context[A])(f: A => Option[B]): Context[B] =
        fa.copy(references = fa.references.view.mapValues(f).collect { case (k, Some(v)) => (k, v) }.to(ListMap))
