package io.taig.otter.munit

import cats.MonadThrow
import cats.syntax.all.*
import munit.Assertions as MunitAssertions
import munit.Location

trait OtterAssertions extends MunitAssertions:
  extension [A, B](self: Either[A, B])
    def assertRight(using Location): B = self match
      case Right(b) => b
      case Left(a)  => fail(s"Expected Right, but got Left: $a")

  extension [F[_]: MonadThrow, A, B](self: F[Either[A, B]])
    def assertSuccess(using Location): F[B] = self.flatMap:
      case Right(b) => b.pure
      case Left(a)  => new IllegalStateException(s"Expected Right, but got Left: $a").raiseError

    def assertError(using Location): F[A] = self.flatMap:
      case Left(a)  => a.pure
      case Right(b) => new IllegalStateException(s"Expected Left, but got Right: $b").raiseError

object OtterAssertions extends OtterAssertions
