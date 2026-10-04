package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.http.Header
import io.taig.otter.http.Query
import io.taig.otter.http.component.HttpComponent.*
import zio.Scope
import zio.test.*

object AbsenceContractTest extends ZIOSpecDefault:
  private def pair(value: String): Chain[(String, Option[String])] = Chain("x" -> Some(value))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("AbsenceContractTest")(
    suite("query contracts")(
      List[(String, Query[Option[Int]], Boolean, Boolean, Chain[(String, Option[String])])](
        ("optional", query("x", int).optional, true, false, Chain.empty),
        ("empty", query("x", int).empty, false, true, pair("")),
        ("optionalOrEmpty", query("x", int).optionalOrEmpty, true, true, Chain.empty),
        ("emptyOrMissing", query("x", int).emptyOrMissing, true, true, pair(""))
      ).map { (name, field, missing, empty, written) =>
        test(name):
          val schema = field.toRecord
          assertTrue(
            QueriesDecoder.decode(schema, Chain.empty).isValid == missing,
            QueriesDecoder.decode(schema, pair("")).isValid == empty,
            QueriesDecoder.decode(schema, Chain("x" -> None)).isValid == empty,
            QueriesDecoder.decode(schema, pair("3")) == Validated.valid(Some(3)),
            QueriesDecoder.decode(schema, pair("bad")).isInvalid,
            QueriesDecoder.decode(schema, pair("3") ++ pair("4")).isInvalid,
            QueriesEncoder.encode(schema, None) == written,
            QueriesEncoder.encode(schema, Some(3)) == pair("3")
          )
      }
    ),
    suite("header contracts")(
      List[(String, Header[Option[Int]], Boolean, Boolean, Chain[(String, String)])](
        ("optional", header("x", int).optional, true, false, Chain.empty),
        ("empty", header("x", int).empty, false, true, Chain("x" -> "")),
        ("optionalOrEmpty", header("x", int).optionalOrEmpty, true, true, Chain.empty),
        ("emptyOrMissing", header("x", int).emptyOrMissing, true, true, Chain("x" -> ""))
      ).map { (name, field, missing, empty, written) =>
        test(name):
          val schema = field.toRecord
          assertTrue(
            HeadersDecoder.decode(schema, Chain.empty).isValid == missing,
            HeadersDecoder.decode(schema, Chain("X" -> "")).isValid == empty,
            HeadersDecoder.decode(schema, Chain("X" -> "3")) == Validated.valid(Some(3)),
            HeadersDecoder.decode(schema, Chain("x" -> "bad")).isInvalid,
            HeadersDecoder.decode(schema, Chain("x" -> "3", "x" -> "4")).isInvalid,
            HeadersEncoder.encode(schema, None) == written
          )
      }
    ),
    test("default triggers do not swallow present invalid values"):
      val missing = query("x", int).defaulted(7).toRecord
      val empty = query("x", int).defaultedOnEmpty(7).toRecord
      val either = query("x", int).defaultedOnMissingOrEmpty(7).toRecord
      assertTrue(
        QueriesDecoder.decode(missing, Chain.empty) == Validated.valid(7),
        QueriesDecoder.decode(missing, pair("")).isInvalid,
        QueriesDecoder.decode(empty, Chain.empty).isInvalid,
        QueriesDecoder.decode(empty, pair("")) == Validated.valid(7),
        QueriesDecoder.decode(either, Chain.empty) == Validated.valid(7),
        QueriesDecoder.decode(either, pair("")) == Validated.valid(7),
        QueriesDecoder.decode(either, pair("bad")).isInvalid,
        QueriesEncoder.encode(either, 7) == pair("7")
      )
    ,
    test("flag keeps Boolean coercions and writes canonical explicit values"):
      val schema = query.flag("x").toRecord
      assertTrue(
        QueriesDecoder.decode(schema, Chain.empty) == Validated.valid(false),
        QueriesDecoder.decode(schema, Chain("x" -> None)) == Validated.valid(true),
        QueriesDecoder.decode(schema, pair("")) == Validated.valid(true),
        QueriesDecoder.decode(schema, pair("false")) == Validated.valid(false),
        QueriesDecoder.decode(schema, pair("yes")) == Validated.valid(true),
        QueriesDecoder.decode(schema, pair("off")) == Validated.valid(false),
        QueriesDecoder.decode(schema, pair("bad")).isInvalid,
        QueriesDecoder.decode(schema, pair("") ++ pair("false")).isInvalid,
        QueriesEncoder.encode(schema, false) == pair("false"),
        QueriesEncoder.encode(schema, true) == pair("true")
      )
    ,
    test("missing repetitions respect optional/default contracts; present empty elements survive"):
      val required = query("x", collection.list(string)).toRecord
      val optional = query("x", collection.list(string)).optional.toRecord
      val default = query("x", collection.list(string)).defaulted(List("fallback")).toRecord
      assertTrue(
        QueriesDecoder.decode(required, Chain.empty) == Validated.valid(Nil),
        QueriesDecoder.decode(optional, Chain.empty) == Validated.valid(None),
        QueriesDecoder.decode(default, Chain.empty) == Validated.valid(List("fallback")),
        QueriesDecoder.decode(optional, pair("")) == Validated.valid(Some(List(""))),
        QueriesDecoder.decode(optional, pair("a") ++ pair("b")) == Validated.valid(Some(List("a", "b"))),
        QueriesEncoder.encode(optional, Some(Nil)) == QueriesEncoder.encode(optional, None)
      )
    ,
    test("optional text preserves present empty text"):
      assertTrue(
        QueriesDecoder.decode(query("x", string).optional.toRecord, pair("")) == Validated.valid(Some("")),
        HeadersDecoder.decode(header("x", string).optional.toRecord, Chain("x" -> "")) == Validated.valid(Some(""))
      )
  )
