package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result
import cats.Invariant

trait Value[+O, A] extends Codec[O, A]:
  override def modifyMetadata(f: Metadata => Metadata): Value[O, A]

  override def modifyDefault(f: Option[A] => Option[A]): Value[O, A]

  override def imap[B](f: A => B)(g: B => A): Value[O, B]

  override def optional: Value[O, Option[A]]

  override def union: Union.Value[this.type, A] = Union.Value(this)

  def parse(value: Option[String]): Codec.Result[A]

  def print(a: A): Option[String]

object Value:
  trait Required[+O, A] extends Value[O, A], Codec.Required[O, A]:
    override def modifyMetadata(f: Metadata => Metadata): Value.Required[O, A]

    override def imap[B](f: A => B)(g: B => A): Value.Required[O, B]

    override def union: Union.Value.Required[this.type, A] = Union.Value.Required(this)

    override def parse(value: Option[String]): Codec.Result[A] = value
      .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String("null"))))
      .andThen(parseValue)

    def parseValue(value: String): Codec.Result[A]

    override def print(a: A): Option[String] = printValue(a).some

    def printValue(a: A): String

  object Required:
    given [O]: Invariant[Value.Required[O, *]] with
      override def imap[A, B](fa: Value.Required[O, A])(f: A => B)(g: B => A): Value.Required[O, B] = fa.imap(f)(g)

  given [O]: Invariant[Value[O, *]] with
    override def imap[A, B](fa: Value[O, A])(f: A => B)(g: B => A): Value[O, B] = fa.imap(f)(g)
