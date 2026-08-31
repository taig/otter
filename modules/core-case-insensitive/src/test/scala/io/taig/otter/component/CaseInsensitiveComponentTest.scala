package io.taig.otter.component

import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Comparison
import io.taig.validation.Validation
import io.taig.validation.cistring.given
import io.taig.validation.std
import org.typelevel.ci.CIString
import zio.Scope
import zio.test.*

import java.util.regex.Pattern

object CaseInsensitiveComponentTest extends ZIOSpecDefault:
  /** The bare AST is a format too, and the only one `core` offers. */
  private given PrimitiveOperation.Text[Primitive.Text]:
    override def string(validation: Validation[Constraint.Primitive.Text, String]): Primitive.Text[String, String] =
      Primitive.Text.Root(validation)

    override def format[W, R](
        name: String,
        parse: String => Either[String, R],
        print: W => String
    ): Primitive.Text[W, R] = Primitive.Text.Format(name, parse, print)

  private object ci extends CaseInsensitiveComponent[Primitive.Text]

  private object refined extends IronComponent.Text[Primitive.Text]

  /** The shape a caller's own constants carry, which is what lets `Match` name a pattern this file did not write. */
  private object Email:
    type Pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
    type Maximum = 254

  private type EmailApiSchema = CIString :| (Match[Email.Pattern] & MaxLength[Email.Maximum])

  /** The two halves of a text primitive, the way an interpreter walks them. A carrier is a `Modify` over the node,
    * unlike a refinement, so reading and writing one is the only way to see that the conversion goes the right way
    * round.
    */
  private def decode[R](schema: Primitive.Text[Nothing, R], value: String): Either[List[Constraint], R] =
    schema match
      case Primitive.Text.Root(validation)   => validation.validate(value).map(_.toList.map(_.constraint)).toLeft(value)
      case Primitive.Text.Modify(self, f, _) => decode(self, value).map(f)
      case Primitive.Text.Format(_, parse, _) => parse(value).leftMap(_ => Nil)

  private def encode[W](schema: Primitive.Text[W, Any], value: W): String = schema match
    case Primitive.Text.Root(_)             => value
    case Primitive.Text.Modify(self, _, g)  => encode(self, g(value))
    case Primitive.Text.Format(_, _, print) => print(value)

  private def constraints(schema: Primitive.Text[Nothing, Any]): List[Constraint.Primitive.Text] = schema match
    case Primitive.Text.Root(validation)   => validation.constraints.toList
    case Primitive.Text.Modify(self, _, _) => constraints(self)
    case Primitive.Text.Format(_, _, _)    => Nil

  private def matches(pattern: String): Constraint.Primitive.Text =
    Constraint.Primitive.Text.Matches(Pattern.compile(pattern))

  private def maximum(reference: Long): Constraint.Primitive.Text =
    Constraint.Primitive.Text.Maximum(Comparison(reference, exclusive = false))

  private def minimum(reference: Long): Constraint.Primitive.Text =
    Constraint.Primitive.Text.Minimum(Comparison(reference, exclusive = false))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CaseInsensitiveComponentTest")(
    test("the text goes in and comes back untouched, the carrier deciding only equality"):
      assertTrue(
        decode(ci.ciString, "AbC") == Right(CIString("AbC")),
        encode(ci.ciString, CIString("AbC")) == "AbC"
      )
    ,
    test("an unvalidated ciString carries no constraint"):
      assertTrue(constraints(ci.ciString).isEmpty)
    ,
    test("a validation stated against CIString is contramapped into the node, not dropped"):
      val schema = ci.ciString(std.text.minimum[CIString](2L))

      assertTrue(
        constraints(schema) === minimum(2L) :: Nil,
        decode(schema, "ab") == Right(CIString("ab")),
        decode(schema, "a").isLeft
      )
    ,
    /** The type this module exists to make expressible, refined through `core-iron` without either module naming the
      * other's vocabulary.
      */
    test("a CIString refines through core-iron, pattern and length together"):
      val schema: Primitive.Text[EmailApiSchema, EmailApiSchema] =
        refined.text[Match[Email.Pattern] & MaxLength[Email.Maximum]](ci.ciString)

      assertTrue(
        constraints(schema) === matches(scala.compiletime.constValue[Email.Pattern]) :: maximum(254L) :: Nil,
        decode(schema, "someone@example.com") == Right(CIString("someone@example.com")),
        decode(schema, "not an email").isLeft,
        decode(schema, "a" * 250 + "@example.com").isLeft
      )
    ,
    /** `Matches[CIString]` runs the pattern against the underlying string, so the carrier decides equality and the
      * pattern decides case.
      */
    test("a refined CIString matches case sensitively, whatever the carrier does with equality"):
      val schema: Primitive.Text[CIString :| Match["[a-z]+"], CIString :| Match["[a-z]+"]] =
        refined.text[Match["[a-z]+"]](ci.ciString)

      assertTrue(decode(schema, "abc").isRight, decode(schema, "ABC").isLeft)
  )
