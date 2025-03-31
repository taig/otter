package io.taig.otter

import cats.implicits.*
import java.math.BigDecimal as JBigDecimal
import java.util.regex.Pattern
import cats.Invariant

abstract class Primitives[S[_]: Invariant]:
  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]] = none,
      maximum: Option[Comparison[JBigDecimal]] = none,
      multiple: Option[JBigDecimal] = none
  ): S[JBigDecimal]

  final def jBigDecimal: S[JBigDecimal] = jBigDecimal(minimum = none, maximum = none, multiple = none)

  def string(
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      matches: Option[Pattern] = none
  ): S[String]

  val string: S[String] = string(minimum = none, maximum = none, matches = none)

  implicit class ToStringCodecOperations(self: string.type) extends StringCodecOperations[S, String] {
    override protected def empty: String = ""
    override protected def isEmpty(a: String): Boolean = a.isEmpty

    def apply(
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        matches: Option[Pattern] = none
    ): S[String] = string(minimum, maximum, matches)
  }

  final val pattern: S[Pattern] = string.imap(Pattern.compile)(_.pattern)

  def parser[A](
      name: String,
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      matches: Option[Pattern] = none
  )(f: String => Either[String, A])(g: A => String): S[A]

object Primitives:
  abstract class Default[S[_]: Invariant] extends Primitives[S]:
    protected def lift[A](codec: Primitive[A]): S[A]

    override def jBigDecimal(
        minimum: Option[Comparison[JBigDecimal]],
        maximum: Option[Comparison[JBigDecimal]],
        multiple: Option[JBigDecimal]
    ): S[JBigDecimal] =
      lift(Primitive.BigDecimal(minimum, maximum, multiple, metadata = Metadata.Empty))

    override def string(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): S[String] =
      lift(Primitive.String(minimum, maximum, matches, metadata = Metadata.Empty))

    override def parser[A](name: String, minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern])(
        f: String => Either[String, A]
    )(g: A => String): S[A] =
      lift(Primitive.Parser(name, decode = f, encode = g, minimum, maximum, matches, metadata = Metadata.Empty))

  object Plain extends Primitives.Default[Primitive]:
    override protected inline def lift[A](codec: Primitive[A]): Primitive[A] = codec
