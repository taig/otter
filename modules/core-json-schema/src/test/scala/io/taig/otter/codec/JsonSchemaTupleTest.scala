package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Reference
import io.taig.otter.Side
import io.taig.otter.Tuple
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

/** The all empty form a tuple admits, which the decoder tests against the whole array rather than per position.
  *
  * `Tuple.Optional` and `Tuple.Default` have no constructor in the DSL, so these build the node directly. They are
  * worth pinning anyway: `Json.absent` is what a renderer asks, and asking it wrongly produces an alternative no
  * document can satisfy.
  */
object JsonSchemaTupleTest extends ZIOSpecDefault:
  private val element: Tuple[Json.Node, Int, Int] = Tuple.Root(Reference.now(int))

  private def decodes(schema: Json.Tuple.Node[?, ?], json: CirceJson): Boolean =
    JsonCirceDecoder.decode(schema, json).isValid

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaTupleTest")(
    test("a position that insists on a value is enough to make the all empty form unreadable"):
      val partial: Tuple[Json.Node, (Int, Option[Int]), (Int, Option[Int])] =
        Tuple.Product(element, Tuple.Optional(element))

      val total: Tuple[Json.Node, (Option[Int], Option[Int]), (Option[Int], Option[Int])] =
        Tuple.Product(Tuple.Optional(element), Tuple.Optional(element))

      assertTrue(
        !Json.absent(Side.Read, partial),
        Json.absent(Side.Read, total)
      )
    ,
    test("what absent says is what the decoder does"):
      val partial = Json.Tuple.Schema(Tuple.Product(element, Tuple.Optional(element)))
      val total = Json.Tuple.Schema(Tuple.Product(Tuple.Optional(element), Tuple.Optional(element)))
      val nulls = CirceJson.arr(CirceJson.Null, CirceJson.Null)

      assertTrue(
        !decodes(partial, nulls),
        decodes(total, nulls),
        JsonCirceDecoder.decode(total, nulls) == Validated.valid((None, None))
      )
    ,
    test("a tuple that admits the empty form says so as a whole tuple, not as a hole in one"):
      val total = Json.Tuple.Schema(Tuple.Product(Tuple.Optional(element), Tuple.Optional(element)))

      assertTrue(
        JsonSchemaRenderer
          .reader(io.taig.otter.JsonSchemaProfile.Draft202012)
          .render(total)
          .value
          .mapObject(_.remove("$schema"))
          .noSpaces ==
          """{"anyOf":[{"type":"array","prefixItems":[{"type":"integer"},{"type":"integer"}],""" +
          """"items":false,"minItems":2,"maxItems":2},{"type":"array","prefixItems":[{"type":"null"},""" +
          """{"type":"null"}],"items":false,"minItems":2,"maxItems":2}]}"""
      )
  )
