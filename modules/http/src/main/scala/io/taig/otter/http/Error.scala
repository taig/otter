package io.taig.otter.http

import io.taig.otter.Violations
import cats.Show
import cats.parse.Parser
import cats.Functor

final case class Error[+A](tpe: A, violations: Option[Violations]):
  def map[B](f: A => B): Error[B] = copy(tpe = f(tpe))

object Error:
  def apply[A](tpe: A, violations: Violations): Error[A] = Error(tpe, Some(violations))

  def apply[A](tpe: A): Error[A] = Error(tpe, None)

  def parse(value: String): Either[Parser.Error, Error[String]] = ???

  given Functor[Error] with
    override def map[A, B](fa: Error[A])(f: A => B): Error[B] = fa.map(f)

  given [A: Show]: Show[Error[A]] = Printers(_)
