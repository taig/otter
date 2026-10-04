package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object FieldContractTest extends ZIOSpecDefault:
  final case class Named(value: Option[Int])

  private def message(value: => Any): String =
    Either.catchOnly[IllegalArgumentException](value).left.toOption.fold("")(_.getMessage)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("FieldContractTest")(
    test("conflicts identify the field and both contracts, even after conversion"):
      val error = message(field("count", int).optional.to[Named].defaulted(Named(None)))
      assertTrue(
        error.contains("count"),
        error.contains("optional(Omitted)"),
        error.contains("defaulted(Missing)"),
        error.contains("schema.nullable")
      )
    ,
    test("all combinations of repeated optional and default contracts fail at construction"):
      assertTrue(
        message(field("x", int).optional.nullable).nonEmpty,
        message(field("x", int).defaulted(1).optional).nonEmpty,
        message(field("x", int).defaulted(1).defaultedOnNull(2)).nonEmpty
      )
    ,
    test("checking a contract does not force its payload or either default"):
      val payload = field("x", sys.error("payload evaluated"): Json[Int]).optional
      val default = field("x", int).defaulted(sys.error("default evaluated"))
      assertTrue(
        payload.isOptional,
        default.isOptional,
        message(default.defaulted(sys.error("second default evaluated"))).contains("already has")
      )
    ,
    test("payload nullability is independent and conversions preserve the contract"):
      val field = io.taig.otter.component.JsonComponent.field("x", int.nullable).optional
      assertTrue(field.isOptional, field.toRecord.self.self.fields.length == 1)
  )
