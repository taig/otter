package io.taig.otter.fixture

import io.circe.parser
import io.taig.otter.JsonBorer
import io.taig.otter.JsonCirce
import zio.Scope
import zio.test.*

import java.math.BigDecimal as JBigDecimal

/** [[CirceDoc.toCirce]] is the one hand written conversion the agreement test rests on, and a wrong one would be wrong
  * in the direction that makes an agreement pass. This is the proof rather than the argument: the same text, through
  * circe's own parser.
  */
object DocTest extends ZIOSpecDefault:
  /** Lexemes both libraries read the same way, which is every one this module cares about except the two in
    * [[DocTest.Divergent]]. `-0`, `412.00`, `1.50`, `4.12e2` and 2^63 are all in here deliberately: they are the cases
    * where borer's parser has thrown formatting away and `JsonBorerNumber` has to recover the value.
    */
  val Lexemes: List[String] = List(
    "0",
    "-0",
    "412",
    "412.0",
    "412.00",
    "4.12e2",
    "-412",
    "0.1",
    "1.50",
    "2147483647",
    "2147483648",
    "9223372036854775807",
    "9223372036854775808",
    "-0.0"
  )

  /** Lexemes the two libraries do *not* agree on, asserted as divergences in
    * [[io.taig.otter.codec.JsonBorerDivergenceTest]].
    *
    * borer's JSON parser refuses an absolute exponent over `Json.DecodingConfig.maxNumberAbsExponent`, 64 by default,
    * where circe reads it and only refuses later. Both libraries cap what a twelve character document may ask for; they
    * cap in different places. `CirceDoc.toCirce` cannot follow circe's parser here either, because
    * `JsonNumber.fromString` and circe's parser disagree about a number this large.
    */
  val Divergent: List[String] = List("1e400", "1e-400")

  private val portable: Gen[Any, Doc] = DocTest.document(DocTest.Lexemes.filter(DocTest.exactlyDouble))

  private val doc: Gen[Any, Doc] = DocTest.document(DocTest.Lexemes)

  /** Whether circe's own parser can be trusted to agree with [[CirceDoc.toCirce]] about a lexeme.
    *
    * On Scala.js it widens a number no `Double` holds exactly -- `9223372036854775807` comes back as
    * `JsonDouble(9223372036854776000)` -- where `JsonNumber.fromString` keeps the lexeme as a `JsonBiggerDecimal`.
    * `CirceDoc.toCirce` is built on the latter and is the more faithful of the two, so the fidelity property is pinned
    * over the lexemes where the parser is itself faithful, and the rest are covered by the agreement test, which both
    * platforms pass.
    */
  private def exactlyDouble(lexeme: String): Boolean =
    new JBigDecimal(lexeme).compareTo(JBigDecimal.valueOf(lexeme.toDouble)) == 0

  private def document(lexemes: List[String]): Gen[Any, Doc] =
    val leaf: Gen[Any, Doc] = Gen.oneOf(
      Gen.const(Doc.Null),
      Gen.boolean.map(Doc.Bool.apply),
      Gen.alphaNumericString.map(Doc.Str.apply),
      Gen.fromIterable(lexemes).map(Doc.Num.apply)
    )

    def nested(depth: Int): Gen[Any, Doc] =
      if depth <= 0 then leaf
      else
        Gen.oneOf(
          leaf,
          Gen.listOfBounded(0, 3)(nested(depth - 1)).map(Doc.Arr.apply),
          Gen.listOfBounded(0, 3)(Gen.alphaNumericStringBounded(1, 6).zip(nested(depth - 1))).map(Doc.Obj.apply)
        )

    nested(depth = 3)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("DocTest")(
    /** Numbers beyond a `Long` are left out, and the reason is worth knowing: circe's *own* parser reads
      * `9223372036854775808` as a `JsonDouble` on Scala.js where `JsonNumber.fromString` keeps it as a
      * `JsonBiggerDecimal`. [[CirceDoc.toCirce]] is the more faithful of the two, so this pins it against the parser
      * everywhere the parser is itself faithful.
      */
    test("circe's own parser reads the text as Doc says circe reads it"):
      check(portable): doc =>
        assertTrue(parser.parse(Doc.render(doc)) == Right(CirceDoc.toCirce(doc)))
    ,
    test("borer's own parser reads the text as the same kind of value circe reads"):
      check(doc): doc =>
        assertTrue(JsonBorer.typeOf(BorerDoc.toBorer(doc)) == JsonCirce.typeOf(CirceDoc.toCirce(doc)))
    ,
    test("every lexeme is a number both libraries accept"):
      assertTrue(DocTest.Lexemes.forall(lexeme => parser.parse(lexeme).isRight))
  )
