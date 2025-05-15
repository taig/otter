package io.taig.otter

import cats.Functor

type Argument[A] = A | Argument.Default

object Argument:
  type Default = Argument.Default.type
  case object Default

  extension [A](self: Argument[A])
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def toOption: Option[A] = self match
      case Argument.Default => None
      case a                => Some(a.asInstanceOf[A])

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def getOrElse[B >: A](b: => B): B = self match
      case Argument.Default => b
      case a                => a.asInstanceOf[B]

  given Functor[Argument] with
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def map[A, B](fa: Argument[A])(f: A => B): Argument[B] = fa match
      case Argument.Default => Argument.Default
      case a                => f(a.asInstanceOf[A])
