package io.taig.otter.codec

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.std
import zio.Scope
import zio.test.*

object JsonSchemaCollectionTest extends ZIOSpecDefault:
  private val schemas: List[(Json.Node[?, ?], Boolean, Boolean)] = List(
    (collection.nonEmptyList(string), true, false),
    (collection.nonEmptyVector(string), true, false),
    (collection.nonEmptyChain(string), true, false),
    (collection.sortedSet(string), false, true),
    (collection.nonEmptySet(string), true, true)
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaCollectionTest")(
    test("intrinsic constraints appear on both sides"):
      val renderers = List(JsonSchemaProfile.Draft202012).flatMap(profile =>
        List(JsonSchemaRenderer.reader(profile), JsonSchemaRenderer.writer(profile))
      )
      assertTrue(
        renderers.forall(renderer =>
          schemas.forall { case (schema, nonEmpty, unique) =>
            val result = renderer.render(schema)
            val cursor = result.value.hcursor
            cursor.get[Long]("minItems").toOption == Option.when(nonEmpty)(1L) &&
            cursor.get[Boolean]("uniqueItems").toOption == Option.when(unique)(true) && result.issues.isEmpty
          }
        )
      )
    ,
    test("conversions keep intrinsic and supplied bounds"):
      val schema = collection
        .nonEmptyList(string, std.collection.maximum[NonEmptyList[String]](10))
        .dimap[NonEmptyList[String], NonEmptyList[String]](identity)(identity)
      val cursor = JsonSchemaRenderer.reader(JsonSchemaProfile.Draft202012).render(schema).value.hcursor
      assertTrue(cursor.get[Long]("minItems").contains(1L), cursor.get[Long]("maxItems").contains(10L))
    ,
    test("a stronger minimum remains conjunctive with the intrinsic minimum"):
      val schema = collection.nonEmptyList(string, std.collection.minimum[NonEmptyList[String]](3))
      val cursor = JsonSchemaRenderer.reader(JsonSchemaProfile.Draft202012).render(schema).value.hcursor
      assertTrue(
        cursor
          .get[List[CirceJson]]("allOf")
          .contains(
            List(
              CirceJson.obj("minItems" -> CirceJson.fromInt(1)),
              CirceJson.obj("minItems" -> CirceJson.fromInt(3))
            )
          )
      )
    ,
    test("an impossible maximum does not replace the intrinsic minimum"):
      val schema = collection.nonEmptyList(string, std.collection.maximum[NonEmptyList[String]](0))
      val cursor = JsonSchemaRenderer.reader(JsonSchemaProfile.Draft202012).render(schema).value.hcursor
      assertTrue(cursor.get[Long]("minItems").contains(1L), cursor.get[Long]("maxItems").contains(0L))
  )
