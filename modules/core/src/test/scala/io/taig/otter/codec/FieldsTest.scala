package io.taig.otter.codec

import cats.data.Chain
import zio.Scope
import zio.test.*

object FieldsTest extends ZIOSpecDefault:
  override val spec: Spec[TestEnvironment & Scope, Any] = suite("FieldsTest")(
    test("a name is taken out of what is left"):
      val (remainders, value) = Fields("title" -> "Dune", "pages" -> "412").take("title")

      assertTrue(value == Some("Dune"), remainders.remainders == Chain("pages" -> "412"))
    ,
    test("a name nothing answers to leaves the fields as they are"):
      val fields = Fields("title" -> "Dune")
      val (remainders, value) = fields.take("pages")

      assertTrue(value == None, remainders.remainders == Chain("title" -> "Dune"))
    ,
    test("a name given twice hands out its first value, then its second"):
      val (rest, first) = Fields("tag" -> "1", "tag" -> "2").take("tag")
      val (remainders, second) = rest.take("tag")

      assertTrue(first == Some("1"), second == Some("2"), remainders.remainders == Chain.empty)
    ,
    test("a name given twice leaves the value nothing claimed where it was"):
      val (remainders, value) = Fields("tag" -> "1", "title" -> "Dune", "tag" -> "2").take("tag")

      assertTrue(value == Some("1"), remainders.remainders == Chain("title" -> "Dune", "tag" -> "2"))
    ,
    test("what nothing claimed is in the order it arrived"):
      val fields = Fields("c" -> 3, "a" -> 1, "b" -> 2)

      assertTrue(fields.take("a")._1.remainders == Chain("c" -> 3, "b" -> 2))
    ,
    test("empty holds nothing"):
      assertTrue(Fields.empty[String].take("title")._2 == None, Fields.empty[String].remainders == Chain.empty)
  )
