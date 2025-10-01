package io.taig.otter

import cats.Applicative
import io.taig.otter.operation.PrimitiveSchemaInvariant
import io.taig.validation.Constraint
import io.taig.validation.Validation

import java.lang.String as String
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.operation.RecordSchemaInvariant
import io.taig.otter as Self

final case class Annotation[+A](metadata: Metadata, self: A):
  def modifyMetadata(f: Metadata => Metadata): Annotation[A] = copy(metadata = f(metadata))

  def map[B](f: A => B): Annotation[B] = copy(self = f(self))

object Annotation:
  def apply[A](self: A): Annotation[A] = Annotation(metadata = Metadata.Empty, self)

  given Applicative[Annotation] with
    override def map[A, B](fa: Annotation[A])(f: A => B): Annotation[B] = fa.copy(self = f(fa.self))
    override def ap[A, B](ff: Annotation[A => B])(fa: Annotation[A]): Annotation[B] =
      Annotation(metadata = fa.metadata ++ ff.metadata, self = ff.self(fa.self))
    override def pure[A](x: A): Annotation[A] = Annotation(self = x)
