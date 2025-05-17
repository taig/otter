package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import io.taig.otter.codec.KeyPrinter
import cats.syntax.all.*
import io.taig.otter.escape
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http.Header
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.KeyParser
import io.taig.otter.unescape
import cats.data.Chain
import io.taig.otter.Violation

object HttpHeaderParser extends Decoder[Http.Header, String]:
  override def decode[A](schema: Header[A], value: String): Validated[Violations, A] = schema match
    case Http.Header.Value(self) => KeyParser.decode(schema = self, value)
    case schema: Http.Header.Array[A] =>
      val values = value.split(",").map(unescape(_, ","))
      HttpHeaderArrayDecoder.decode(schema, Chain.fromIterableOnce(values))
    case schema: Http.Header.Object[A] =>
      parser
        .obj(value)
        .toValidated
        .leftMap(error => Violations.rootNec(Violation.tpe(name = "object", actual = value, hint = error.show)))
        .andThen(values => HttpHeaderObjectDecoder.decode(schema, Chain.fromSeq(values)))

  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    val token = (character: Char) =>
      charWhere(value => value != '\\' && value != character).orElse(char('\\') *> anyChar)

    val obj: String => Either[Error, List[(String, Option[String])]] =
      val key = token('=').rep.string
      val value = token(',').rep0.string
      val parser = (key ~ (char('=') *> value).?).repSep0(char(','))
      (value: String) =>
        parser.parseAll(value).map(_.map(_.bimap(unescape(_, "="), _.map(unescape(_, List(",", "="))))))
