package io.taig.otter

import cats.Functor

type Argument[A] = A | Argument.Default

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
object Argument:
  type Default = Argument.Default.type
  case object Default

  extension [A](self: Argument[A])
    def toOption: Option[A] = self match
      case Argument.Default => None
      case a                => Some(a.asInstanceOf[A])

    def getOrElse[B >: A](b: => B): B = self match
      case Argument.Default => b
      case a                => a.asInstanceOf[B]

  given Functor[Argument] with
    def map[A, B](fa: Argument[A])(f: A => B): Argument[B] = fa match
      case Argument.Default => Argument.Default
      case a                => f(a.asInstanceOf[A])
