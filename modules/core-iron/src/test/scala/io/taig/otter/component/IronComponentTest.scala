package io.taig.otter.component

import cats.data.Chain
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Comparison
import io.taig.validation.Validation
import zio.Scope
import zio.test.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object IronComponentTest extends ZIOSpecDefault:
  /** The bare AST is a format too, and the only one `core` offers. */
  private given PrimitiveOperation.Number[Primitive.Number]:
    override def bigDecimal(
        validation: Validation[Constraint.Primitive.Number, JBigDecimal]
    ): Primitive.Number[JBigDecimal, JBigDecimal] = Primitive.Number.BigDecimal(validation)

    override def bigInteger(
        validation: Validation[Constraint.Primitive.Number, JBigInteger]
    ): Primitive.Number[JBigInteger, JBigInteger] = Primitive.Number.BigInteger(validation)

    override def double(validation: Validation[Constraint.Primitive.Number, Double]): Primitive.Number[Double, Double] =
      Primitive.Number.Double(validation)

    override def float(validation: Validation[Constraint.Primitive.Number, Float]): Primitive.Number[Float, Float] =
      Primitive.Number.Float(validation)

    override def int(validation: Validation[Constraint.Primitive.Number, Int]): Primitive.Number[Int, Int] =
      Primitive.Number.Int(validation)

    override def long(validation: Validation[Constraint.Primitive.Number, Long]): Primitive.Number[Long, Long] =
      Primitive.Number.Long(validation)

  private given PrimitiveOperation.Text[Primitive.Text]:
    override def string(validation: Validation[Constraint.Primitive.Text, String]): Primitive.Text[String, String] =
      Primitive.Text.Root(validation)

    override def format[W, R](
        name: String,
        parse: String => Either[String, R],
        print: W => String
    ): Primitive.Text[W, R] = Primitive.Text.Format(name, parse, print)

  private given [S[-w, +r] <: Primitive.Text[w, r]] => CollectionOperation[[w, r] =>> Collection[S, w, r], S]:
    override def chained[W, R](
        schema: Reference[S, W, R],
        validation: Validation[Constraint.Collection, Chain[R]]
    ): Collection[S, Chain[W], Chain[R]] = Collection.Chained(schema, validation)

    override def indexed[W, R](
        schema: Reference[S, W, R],
        validation: Validation[Constraint.Collection, Vector[R]]
    ): Collection[S, Vector[W], Vector[R]] = Collection.Indexed(schema, validation)

    override def linked[W, R](
        schema: Reference[S, W, R],
        validation: Validation[Constraint.Collection, List[R]]
    ): Collection[S, List[W], List[R]] = Collection.Linked(schema, validation)

    extension [W, R](fa: Collection[S, W, R]) override def schema: Reference[S, ?, ?] = fa.schema

  private object refined
      extends IronComponent.Number[Primitive.Number],
        IronComponent.Text[Primitive.Text],
        IronComponent.Collection[Primitive.Text, [s[-w, +r] <: Primitive.Text[w, r], w, r] =>> Collection[s, w, r]]

  private val string: Primitive.Text[String, String] = Primitive.Text.Root(Validation.valid)

  /** The refined type is what a caller gets, which is why every schema below is ascribed rather than inferred: the
    * ascription is the assertion, and it either compiles or it does not.
    */
  private def check[S, A](
      validation: Option[Validation[S, A]],
      constraints: List[S],
      accepted: A,
      rejected: A
  ): TestResult = validation match
    case Some(validation) =>
      assertTrue(
        validation.constraints.toList == constraints,
        validation.validate(accepted).isEmpty,
        validation.validate(rejected).isDefined
      )
    case None => assertTrue(false)

  /** Every extractor names one node and nothing else, so a schema that arrived wrapped in a `Modify` falls through to
    * `None` and fails the test: the refinement is a cast, not a mapping, and the node has to come back untouched.
    */
  private def int(schema: Primitive.Number[Nothing, Int]): Option[Validation[Constraint.Primitive.Number, Int]] =
    schema match
      case Primitive.Number.Int(validation) => validation.some
      case _                                => none

  private def long(schema: Primitive.Number[Nothing, Long]): Option[Validation[Constraint.Primitive.Number, Long]] =
    schema match
      case Primitive.Number.Long(validation) => validation.some
      case _                                 => none

  private def double(
      schema: Primitive.Number[Nothing, Double]
  ): Option[Validation[Constraint.Primitive.Number, Double]] = schema match
    case Primitive.Number.Double(validation) => validation.some
    case _                                   => none

  private def float(schema: Primitive.Number[Nothing, Float]): Option[Validation[Constraint.Primitive.Number, Float]] =
    schema match
      case Primitive.Number.Float(validation) => validation.some
      case _                                  => none

  private def text(schema: Primitive.Text[Nothing, String]): Option[Validation[Constraint.Primitive.Text, String]] =
    schema match
      case Primitive.Text.Root(validation) => validation.some
      case _                               => none

  private def chained(
      schema: Collection[Primitive.Text, Nothing, Chain[String]]
  ): Option[Validation[Constraint.Collection, Chain[String]]] = schema match
    case Collection.Chained(_, validation) => validation.some
    case _                                 => none

  private def indexed(
      schema: Collection[Primitive.Text, Nothing, Vector[String]]
  ): Option[Validation[Constraint.Collection, Vector[String]]] = schema match
    case Collection.Indexed(_, validation) => validation.some
    case _                                 => none

  private def linked(
      schema: Collection[Primitive.Text, Nothing, List[String]]
  ): Option[Validation[Constraint.Collection, List[String]]] = schema match
    case Collection.Linked(_, validation) => validation.some
    case _                                => none

  private def numberMinimum(reference: Int, exclusive: Boolean): Constraint.Primitive.Number =
    Constraint.Primitive.Number.Minimum(Comparison(reference, exclusive))

  private def numberMaximum(reference: Int, exclusive: Boolean): Constraint.Primitive.Number =
    Constraint.Primitive.Number.Maximum(Comparison(reference, exclusive))

  private def textMinimum(reference: Long, exclusive: Boolean): Constraint.Primitive.Text =
    Constraint.Primitive.Text.Minimum(Comparison(reference, exclusive))

  private def textMaximum(reference: Long, exclusive: Boolean): Constraint.Primitive.Text =
    Constraint.Primitive.Text.Maximum(Comparison(reference, exclusive))

  private def collectionMinimum(reference: Long, exclusive: Boolean): Constraint.Collection =
    Constraint.Collection.Minimum(Comparison(reference, exclusive))

  private def collectionMaximum(reference: Long, exclusive: Boolean): Constraint.Collection =
    Constraint.Collection.Maximum(Comparison(reference, exclusive))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("IronComponentTest")(
    test("int carries the derived bound"):
      val schema: Primitive.Number[Int :| Greater[0], Int :| Greater[0]] = refined.int[Greater[0]]
      check(int(schema), numberMinimum(0, exclusive = true) :: Nil, accepted = 1, rejected = 0)
    ,
    test("an intersection contributes one constraint per member, in declaration order"):
      val schema: Primitive.Number[Int :| (Greater[0] & Less[100]), Int :| (Greater[0] & Less[100])] =
        refined.int[Greater[0] & Less[100]]
      check(
        int(schema),
        numberMinimum(0, exclusive = true) :: numberMaximum(100, exclusive = true) :: Nil,
        accepted = 50,
        rejected = 100
      )
    ,
    test("long carries the derived bound"):
      val schema: Primitive.Number[Long :| GreaterEqual[1L], Long :| GreaterEqual[1L]] = refined.long[GreaterEqual[1L]]
      check(
        long(schema),
        Constraint.Primitive.Number.Minimum(Comparison(1L, exclusive = false)) :: Nil,
        accepted = 1L,
        rejected = 0L
      )
    ,
    test("double carries the derived bound"):
      val schema: Primitive.Number[Double :| Greater[0.0], Double :| Greater[0.0]] = refined.double[Greater[0.0]]
      check(
        double(schema),
        Constraint.Primitive.Number.Minimum(Comparison(0.0, exclusive = true)) :: Nil,
        accepted = 0.5,
        rejected = 0.0
      )
    ,
    test("float carries the derived bound"):
      val schema: Primitive.Number[Float :| Less[1.0f], Float :| Less[1.0f]] = refined.float[Less[1.0f]]
      check(
        float(schema),
        Constraint.Primitive.Number.Maximum(Comparison(1.0f, exclusive = true)) :: Nil,
        accepted = 0.5f,
        rejected = 1.0f
      )
    ,
    test("string carries the derived length bounds"):
      val schema: Primitive.Text[String :| (MinLength[1] & MaxLength[3]), String :| (MinLength[1] & MaxLength[3])] =
        refined.string[MinLength[1] & MaxLength[3]]
      check(
        text(schema),
        textMinimum(1L, exclusive = false) :: textMaximum(3L, exclusive = false) :: Nil,
        accepted = "abc",
        rejected = ""
      )
    ,
    test("chain carries the derived size bounds"):
      val schema: Collection[
        Primitive.Text,
        Chain[String] :| MinLength[1],
        Chain[String] :| MinLength[1]
      ] = refined.chain[MinLength[1]](string)
      check(
        chained(schema),
        collectionMinimum(1L, exclusive = false) :: Nil,
        accepted = Chain.one("a"),
        rejected = Chain.empty
      )
    ,
    test("vector carries the derived size bounds"):
      val schema: Collection[
        Primitive.Text,
        Vector[String] :| MinLength[1],
        Vector[String] :| MinLength[1]
      ] = refined.vector[MinLength[1]](string)
      check(
        indexed(schema),
        collectionMinimum(1L, exclusive = false) :: Nil,
        accepted = Vector("a"),
        rejected = Vector.empty
      )
    ,
    test("list carries the derived size bounds"):
      val schema: Collection[
        Primitive.Text,
        List[String] :| (MinLength[1] & MaxLength[3]),
        List[String] :| (MinLength[1] & MaxLength[3])
      ] = refined.list[MinLength[1] & MaxLength[3]](string)
      check(
        linked(schema),
        collectionMinimum(1L, exclusive = false) :: collectionMaximum(3L, exclusive = false) :: Nil,
        accepted = List("a", "b"),
        rejected = Nil
      )
    ,
    /** The element schema is what it was handed, unwrapped and suspended the way the plain component suspends it. */
    test("a refined collection keeps its element schema"):
      val schema: Collection[Primitive.Text, List[String] :| MinLength[1], List[String] :| MinLength[1]] =
        refined.list[MinLength[1]](string)
      assertTrue(schema.schema.value == string)
  )
