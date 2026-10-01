package io.taig.otter.http

import io.taig.otter.http.component.HttpComponent.*
import scodec.bits.ByteVector
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

object ResponseConversionTest extends ZIOSpecDefault:
  enum Empty:
    case Removed, Missing

  enum Mixed:
    case Removed
    case Content(bytes: ByteVector)

  enum Three:
    case Removed, Missing, Unchanged

  enum Extended:
    case Removed, Missing
    case Content(bytes: ByteVector)

  val removed = response(status.noContent).to[ResponseConversionTest.Empty.Removed.type]
  val missing = response(status.notFound).to[ResponseConversionTest.Empty.Missing.type]
  val alternatives = removed :+ missing
  val empty = alternatives.to[ResponseConversionTest.Empty]
  val mixed = (response(status.noContent).to[ResponseConversionTest.Mixed.Removed.type] :+
    response(status.ok)(body.binary(mediaType.octetStream)).to[ResponseConversionTest.Mixed.Content])
    .to[ResponseConversionTest.Mixed]

  val three = (response(status.noContent).to[ResponseConversionTest.Three.Removed.type] :+
    response(status.notFound).to[ResponseConversionTest.Three.Missing.type] :+
    response(status.notModified).to[ResponseConversionTest.Three.Unchanged.type]).to[ResponseConversionTest.Three]

  val extended = (response(status.noContent).to[ResponseConversionTest.Extended.Removed.type] :+
    response(status.notFound).to[ResponseConversionTest.Extended.Missing.type] :+
    response(status.ok)(body.binary(mediaType.octetStream)).to[ResponseConversionTest.Extended.Content])
    .to[ResponseConversionTest.Extended]

  val reader = response(status.noContent).map(identity).mapTo[ResponseConversionTest.Empty.Removed.type]
  val writer =
    response(status.noContent).contramap[Unit](identity).contramapTo[ResponseConversionTest.Empty.Removed.type]

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ResponseConversionTest")(
    test("singleton and all-bodyless conversions preserve the empty requirement"):
      val single: Response.Schema[
        Nothing,
        ResponseConversionTest.Empty.Removed.type,
        ResponseConversionTest.Empty.Removed.type
      ] = removed
      val union: Responses.Of[Nothing, ResponseConversionTest.Empty] = empty
      assertTrue(
        single.status == status.noContent,
        Responses.branches(union).map(_.status).toList == List(status.noContent, status.notFound)
      )
    ,
    test("an inferred mixed union retains its body requirement"):
      val union: Responses.Of[Body.Opaque, ResponseConversionTest.Mixed] = mixed
      assertTrue(Responses.branches(union).map(_.status).toList == List(status.noContent, status.ok))
    ,
    test("appending a third bodyless case retains Nothing"):
      val union: Responses.Of[Nothing, ResponseConversionTest.Three] = three
      assertTrue(
        Responses.branches(union).map(_.status).toList == List(status.noContent, status.notFound, status.notModified)
      )
    ,
    test("a body added after bodyless alternatives contributes its requirement"):
      val union: Responses.Of[Body.Opaque, ResponseConversionTest.Extended] = extended
      assertTrue(Responses.branches(union).map(_.status).toList == List(status.noContent, status.notFound, status.ok))
    ,
    test("directional conversions retain only their original capability"):
      val read: Response.Reader.Of[Nothing, ResponseConversionTest.Empty.Removed.type] = reader
      val write: Response.Writer.Of[Nothing, ResponseConversionTest.Empty.Removed.type] = writer
      assertTrue(read.status == status.noContent, write.status == status.noContent)
    ,
    test("a converted reader cannot become a round trip"):
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        val invalid: Response.Of[Nothing, ResponseConversionTest.Empty.Removed.type] = ResponseConversionTest.reader
      """))
    ,
    test("a converted writer cannot become a round trip"):
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        val invalid: Response.Of[Nothing, ResponseConversionTest.Empty.Removed.type] = ResponseConversionTest.writer
      """))
    ,
    test("a mixed union cannot discard its body requirement"):
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        val invalid: Responses.Of[Nothing, ResponseConversionTest.Mixed] = ResponseConversionTest.mixed
      """))
  )
