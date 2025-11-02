package io.taig.otter.syntax

import cats.Invariant
import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.operation.UnionOperation

import scala.reflect.TypeTest
import io.taig.otter.Branch

trait UnionSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: UnionOperation[Self, Value])
    def orElse[B](schema: Self[B]): Self[Either[A, B]] = operation.orElse(self, schema)

    def :+[B](schema: Self[B]): Self[Either[A, B]] = operation.orElse(self, schema)

    def schemas: NonEmptyChain[Branch[Value, ?]] = operation.branches(self)

  extension [Self[_]: Invariant, Value[_], A](self: Self[A])(using UnionOperation[Self, Value])
    def |[B](schema: Self[B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Self[A | B] =
      (self :+ schema).imap(_.merge):
        case a: A => Left(a)
        case b: B => Right(b)

object UnionSyntax extends UnionSyntax
