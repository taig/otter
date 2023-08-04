package io.taig.otter.http

import cats.{Eval, Invariant}
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.Schema
import io.taig.otter.validation.Validation

sealed abstract class Input[A]:
  def method: Method
  def url: Url[?]
//  def headers: Headers[?]
  def body: Input.Body[?]

object Input:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }
    def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
    final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
    final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  object Body:
    sealed abstract class Singlepart[A] extends Input.Body[A]:
      self =>
      override type Self[a] <: Input.Body.Singlepart[a] { type Self[a] = self.Self[a] }

    object Singlepart:
      sealed abstract class Strict[A] extends Input.Body.Singlepart[A]:
        override type Self[a] = Input.Body.Singlepart.Strict[a]
        final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Input.Body.Singlepart.Strict[B] =
          Strict.Validate(this, validation, g)

      object Strict:
        private[otter] case object Bytes extends Input.Body.Singlepart.Strict[Array[Byte]]

        final private[otter] case class Validate[A, B](
            self: Input.Body.Singlepart.Strict[A],
            validation: Validation[A, B],
            g: B => A
        ) extends Input.Body.Singlepart.Strict[B]
