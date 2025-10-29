package io.taig.otter.syntax

import cats.Invariant
import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Reference
import io.taig.otter.operation.UnionOperation

import scala.annotation.targetName
import scala.reflect.TypeTest

trait UnionSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: UnionOperation[Self, Value])
    def orElse[B](schema: Self[B]): Self[Either[A, B]] =
      operation.orElse(self, schema)

    def :+[B](schema: Value[B]): Self[Either[A, B]] = operation.orElse(self, operation.lift(schema))

    // def schemas: NonEmptyChain[Reference[Value, ?]] = operation.schemas(self)

  extension [Self[_], Value[_], A](self: Self[A])
    def :+[B](schema: Value[B])(using operation: UnionOperation[Self, Value]): Self[Either[A, B]] =
      operation.orElse(self, operation.lift(schema))

  // extension [Self[_]: Invariant, Value[_], A](self: Self[A])(using UnionOperation[Self, Value])
  //   def |[B](schema: Value[B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Self[A | B] =
  //     (self :+ schema).imap(_.merge):
  //       case a: A => Left(a)
  //       case b: B => Right(b)

  extension [Self[_], Value[_], A](self: Value[A])(using operation: UnionOperation[Self, Value])
    def toUnion: Self[A] = operation.lift(self)

    // def :+[B](schema: Value[B]): Self[Either[A, B]] = operation.orElse(operation.lift(self), operation.lift(schema))

  // extension [Self[_]: Invariant, Value[_], A](self: Value[A])(using operation: UnionOperation[Self, Value])
  //   def |[B](schema: Value[B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Self[A | B] = (self :+ schema).imap(_.merge):
  //       case a: A => Left(a)
  //       case b: B => Right(b)

object UnionSyntax extends UnionSyntax
