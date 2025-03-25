package io.taig.otter

import munit.FunSuite
import io.taig.otter.http.Dsl.*
import io.taig.otter.http.json.Codecs.json
import org.typelevel.ci.*
import cats.syntax.all.*
import scala.collection.immutable.ListMap

final class DataEndpointPrinterTest extends FunSuite:
  test("yolo"):
    val x = endpoint(
      request(
        method = method.get,
        url = __ / "yolo" / parameter("lol", string) / parameter("asdf", int),
        headers = header(ci"Yolo Lol", string) :* header(ci"X-Api-Key", string.nullable),
        bodies = text :+ binary :+ json(field("a", string) :* field("b", int))
      ),
      response(
        result(code.ok)
      )
    )

    val printer = DataEndpointPrinter(codecs = ZodCodecPrinter):
      case (mediaType.text.plain, _)              => ("z.string()", "(response) => response.text()")
      case (mediaType.application.octetStream, _) => ("z.instanceof(Uint8Array)", "(response) => response.text()")
      case (mediaType.application.json, Some(codec)) =>
        (ZodCodecPrinter.print(codec).runA(ListMap.empty).value.show, "(response) => response.text()")

    println(printer.print(x))
