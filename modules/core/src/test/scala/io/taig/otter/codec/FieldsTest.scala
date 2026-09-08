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
    ,
    /** A position indexes what arrived, not what the schema names, so a document with more than thirty-two members
      * reaches the overflow set however narrow the schema reading it. Thirty-three keys is an ordinary JSON object, a
      * wide CSV header or a long query string -- not something only an enormous one does.
      */
    suite("past the thirty-second position")(
      test("a name beyond the mask is found and claimed"):
        val fields = Fields.from((0 until 40).map(index => s"f$index" -> index))
        val (remainders, value) = fields.take("f35")

        assertTrue(value == Some(35), remainders.take("f35")._2 == None, remainders.remainders.size == 39L)
      ,
      test("a name given twice beyond the mask hands out each occurrence in turn"):
        val entries = (0 until 40).map(index => s"f$index" -> index) ++ Seq("f35" -> 99)
        val (once, first) = Fields.from(entries).take("f35")
        val (twice, second) = once.take("f35")

        assertTrue(first == Some(35), second == Some(99), twice.take("f35")._2 == None)
      ,
      test("claiming across the boundary leaves every other position where it was"):
        val fields = Fields.from((0 until 40).map(index => s"f$index" -> index))
        val claimed = List("f0", "f31", "f32", "f39").foldLeft(fields)((rest, name) => rest.take(name)._1)

        assertTrue(
          claimed.remainders.size == 36L,
          !claimed.remainders.toList.map(_._1).exists(Set("f0", "f31", "f32", "f39")),
          claimed.take("f1")._2 == Some(1)
        )
    )
  )
