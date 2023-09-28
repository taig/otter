package io.taig.otter
import cats.data.{Chain, Validated}
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation, Violations}

sealed abstract class Enumeration[A](description: Option[String]) extends Schema[A](description) with Value[A]:
  final override type Self[a] = Enumeration[a]
  final override def description(f: Option[String] => Option[String]): Enumeration[A] =
    Enumeration(this, f(description))
  final override def optional: Enumeration[Option[A]] = ???
  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] = ???

object Enumeration:
  def apply[A](schema: Enumeration[A], description: Option[String]): Enumeration[A] =
    new Enumeration[A](description) { export schema.* }

  def apply[A, B](schema: Value[A], mapping: Mapping[B, A]): Enumeration[B] = new Enumeration[B](None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Data): Validated[Violations, B] = ???
    override def encode(b: B): Data = schema.encode(mapping.inj(b))
    override def parse(value: Option[String]): Validated[Violations, B] = ???
    override def print(b: B): Option[String] = schema.print(mapping.inj(b))
