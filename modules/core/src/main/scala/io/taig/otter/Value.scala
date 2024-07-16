package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result

abstract class Value[+O, A] extends Codec[O, A]:
  override def imap[B](f: A => B)(g: B => A): Value[O, B]
  override def optional: Value[O, Option[A]]
  override def update(f: Metadata => Metadata): Value[O, A]

  def parse(value: Option[String]): Codec.Result[String, A]

  def print(a: A): Option[String]

object Value:
  trait Required[+O, A] extends Value[O, A]:
    override def imap[B](f: A => B)(g: B => A): Value.Required[O, B]
    override def optional: Value[O, Option[A]]
    override def update(f: Metadata => Metadata): Value.Required[O, A]

    final override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, A] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type("value"), actual = Data.String("null"))))
      .andThen(decodeValue)

    def decodeValue(data: Data.Value): Codec.Result[Data, A]

    final override def encodeOption(a: A): Option[Data.Value] = encodeValue(a).some

    def encodeValue(a: A): Data.Value

    override def parse(value: Option[String]): Codec.Result[String, A] = value
      .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = "null")))
      .andThen(parseValue)

    def parseValue(value: String): Codec.Result[String, A]

    override def print(a: A): Option[String] = printValue(a).some

    def printValue(a: A): String
