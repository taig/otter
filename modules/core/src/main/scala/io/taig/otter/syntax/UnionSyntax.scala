package io.taig.otter.syntax

import io.taig.otter.operation.UnionOperation
import scala.annotation.targetName
import cats.syntax.all.*
import cats.Invariant
import scala.reflect.TypeTest

trait UnionSyntax[Self[_]: Invariant, Value[_]](using operation: UnionOperation[Self, Value]):
  extension [A](self: Self[A])
    def orElse[B](schema: Self[B]): Self[Either[A, B]] =
      operation.orElse(self, schema)

    def :+[B](schema: Value[B]): Self[Either[A, B]] = operation.orElse(self, operation.lift(schema))

    def |[B](schema: Value[B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Self[A | B] = (self :+ schema).imap(_.merge):
      case a: A => Left(a)
      case b: B => Right(b)

  extension [A](self: Value[A])
    def toUnion: Self[A] = operation.lift(self)

    @targetName("_:+")
    def :+[B](schema: Value[B]): Self[Either[A, B]] = operation.orElse(operation.lift(self), operation.lift(schema))

    @targetName("_|")
    def |[B](schema: Value[B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Self[A | B] = (self :+ schema).imap(_.merge):
      case a: A => Left(a)
      case b: B => Right(b)