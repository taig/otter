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
  )
