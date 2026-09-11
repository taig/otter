package io.taig.otter

import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

object MetadataTest extends ZIOSpecDefault:
  override val spec: Spec[TestEnvironment & Scope, Any] = suite("MetadataTest")(
    test("one rejects a value of a different type from its key"):
      assertTrue(!typeChecks("""
        val key = Metadata.Key[Int]("count")
        Metadata.one(Metadata.Namespace.Global, key, "oops")
      """))
    ,
    test("put rejects a value of a different type from its key"):
      assertTrue(!typeChecks("""
        val key = Metadata.Key[Int]("count")
        Metadata.Empty.put(Metadata.Namespace.Global, key, "oops")
      """))
    ,
    test("global attr rejects a value of a different type from its key"):
      assertTrue(!typeChecks("""
        val annotated = Annotated[Annotation[Unit]]
        import annotated.*
        val key = Metadata.Key[Int]("count")
        Annotation(()).attr(key, "oops")
      """))
    ,
    test("namespaced attr rejects a value of a different type from its key"):
      assertTrue(!typeChecks("""
        val annotated = Annotated[Annotation[Unit]]
        import annotated.*
        val key = Metadata.Key[Int]("count")
        Annotation(()).attr(Metadata.Namespace.Global, key, "oops")
      """))
    ,
    test("a key cannot be widened to allow incompatible writes"):
      assertTrue(!typeChecks("""
        val key = Metadata.Key[Int]("count")
        val widened: Metadata.Key[Any] = key
        Metadata.one(Metadata.Namespace.Global, widened, "oops")
      """))
    ,
    test("typed keys store and retrieve heterogeneous values and accept subtypes"):
      val count = Metadata.Key[Int]("count")
      val names = Metadata.Key[Seq[String]]("names")
      val namespace = Metadata.Namespace("test")
      val metadata = Metadata
        .one(Metadata.Namespace.Global, count, 1)
        .put(namespace, names, List("Otter"))
      val updated = metadata ++ Metadata.one(Metadata.Namespace.Global, count, 2)

      assertTrue(
        metadata.get(Metadata.Namespace.Global, count).contains(1),
        metadata.get(namespace, names).contains(List("Otter")),
        metadata.get(namespace, Metadata.Namespace.Global, count).contains(1),
        updated.get(Metadata.Namespace.Global, count).contains(2),
        metadata.contains(namespace, names),
        !metadata.remove(namespace, names).contains(namespace, names),
        metadata.toSortedMap.size == 2
      )
    ,
    test("attr accepts values matching the key type and subtypes"):
      val annotated = Annotated[Annotation[Unit]]
      import annotated.*

      val count = Metadata.Key[Int]("count")
      val names = Metadata.Key[Seq[String]]("names")
      val namespace = Metadata.Namespace("test")
      val annotation = Annotation(()).attr(count, 1).attr(namespace, names, List("Otter"))

      assertTrue(annotation.attr(count).contains(1), annotation.attr(namespace, names).contains(List("Otter")))
    ,
    test("attr writes a collection valued key from its elements, whatever collection the key holds"):
      val annotated = Annotated[Annotation[Unit]]
      import annotated.*

      val tags = Metadata.Key[List[String]]("tags")
      val names = Metadata.Key[Seq[String]]("names")
      val codes = Metadata.Key[Set[Int]]("codes")
      val labels = Metadata.Key[Vector[String]]("labels")
      val headers = Metadata.Key[Map[String, String]]("headers")
      val namespace = Metadata.Namespace("test")

      val annotation = Annotation(())
        .attr(tags, "books", "manuals")
        .attr(names, "Otter")
        .attr(codes, 200, 404, 200)
        .attr(labels, "one", "two")
        .attr(headers, "Accept" -> "application/json", "Accept-Language" -> "en")
        .attr(namespace, tags, "loans")

      assertTrue(
        annotation.attr(tags).contains(List("books", "manuals")),
        annotation.attr(names).contains(Seq("Otter")),
        annotation.attr(codes).contains(Set(200, 404)),
        annotation.attr(labels).contains(Vector("one", "two")),
        annotation.attr(headers).contains(Map("Accept" -> "application/json", "Accept-Language" -> "en")),
        annotation.attr(namespace, tags).contains(List("loans")),
        Annotation(()).attr(tags, Nil).attr(tags).contains(Nil)
      )
    ,
    test("the varargs attr rejects elements of a different type from its key"):
      assertTrue(!typeChecks("""
        val annotated = Annotated[Annotation[Unit]]
        import annotated.*
        val tags = Metadata.Key[List[String]]("tags")
        Annotation(()).attr(tags, "books", 1)
      """))
    ,
    test("a collection valued key is still written whole, and a key that holds no collection is unaffected"):
      val annotated = Annotated[Annotation[Unit]]
      import annotated.*

      val nested = Metadata.Key[List[List[String]]]("nested")
      val count = Metadata.Key[Int]("count")
      val name = Metadata.Key[String]("name")
      val annotation = Annotation(())
        .attr(nested, List(List("books"), List("loans")))
        .attr(count, 1)
        .attr(name, "Otter")

      assertTrue(
        annotation.attr(nested).contains(List(List("books"), List("loans"))),
        annotation.attr(count).contains(1),
        annotation.attr(name).contains("Otter")
      )
  )
