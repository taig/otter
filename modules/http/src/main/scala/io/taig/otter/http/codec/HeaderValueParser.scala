package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.unescape
import io.taig.otter.http.Header

object HeaderValueParser extends Decoder[Header.Value, String]:
  override def decode[A](schema: Header.Value[A], value: String): Validated[Violations, A] = schema match
    case schema: Header.Value.Atom[A] => HeaderValueAtomParser.decode(schema, value)
    case schema: Header.Value.Array[A] =>
      val values = value.split(",").map(unescape(_, ","))
      HeaderValueArrayDecoder.decode(schema, Chain.fromIterableOnce(values))
    case schema: Header.Value.Object[A] =>
      parser
        .obj(value)
        .toValidated
        .leftMap(error => Violations.rootNec(Violation.tpe(name = "object", actual = value, hint = error.show)))
        .andThen(values => HeaderValueObjectDecoder.decode(schema, Chain.fromSeq(values)))

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
