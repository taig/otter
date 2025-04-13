package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

final class HttpHeaderParser(explode: Boolean) extends Parser[Http.Header]:
  override def apply[A](codec: Http.Header[A], value: String): Validated[Violations, A] = codec match
    case codec: Http.Header.Array[A]  => apply(codec, value)
    case codec: Http.Header.Object[A] => apply(codec, value)
    case codec: Http.Header.Value[A]  => HttpHeaderValueParser(codec, value)

  def apply[A](codec: Http.Header.Array[A], value: String): Validated[Violations, A] =
    HttpHeaderParser.parser
      .array(value)
      .toValidatedViolations(tpe = "array", value)
      .andThen: values =>
        codec match
          case Http.Header.Array.Collection(self) =>
            CollectionParser(parser = HttpHeaderValueParser)(codec = self, values)
          case Http.Header.Array.Tuple(self) =>
            TupleParser(parser = HttpHeaderValueParser)(codec = self, values)

  def apply[A](codec: Http.Header.Object[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Object.Dictionary(self) => apply(codec = self, value)
    case Http.Header.Object.Record(self)     => apply(codec = self, value)

  def apply[A](codec: Dictionary[Http.Header.Value, Http.Header.Value, A], value: String): Validated[Violations, A] =
    obj(value).andThen(DictionaryParser(parser = HttpHeaderValueParser)(codec, _))

  def apply[A](codec: Record[Http.Header.Value, Http.Header.Value, A], value: String): Validated[Violations, A] =
    obj(value).andThen(
      RecordParser(parser = HttpHeaderValueParser, printer = HttpHeaderValuePrinter)(codec, _).map((_, a) => a)
    )

  def obj(value: String): Validated[Violations, List[(String, String)]] =
    val values =
      if explode
      then HttpHeaderParser.parser.obj.exploded(value)
      else HttpHeaderParser.parser.obj.unexploded(value)

    values.toValidatedViolations(tpe = "object", value)

object HttpHeaderParser:
  def apply(explode: Boolean = false): Parser[Http.Header] = new HttpHeaderParser(explode)

  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    val token = (character: Char) =>
      charWhere(value => value != '\\' && value != character).orElse(char('\\') *> anyChar)

    val array: String => Either[Error, List[String]] =
      val parser = token(',').rep.string.repSep0(char(','))
      (value: String) => parser.parseAll(value).map(_.map(unescape(_, ",")))

    object obj:
      val exploded: String => Either[Error, List[(String, String)]] =
        val key = token('=').rep0.string
        val value = token(',').rep0.string
        val parser = (key.with1 ~ (char('=') *> value)).repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, "="), unescape(_, List(",", "=")))))

      val unexploded: String => Either[Error, List[(String, String)]] =
        val value = token(',').rep0.string
        val parser = (value.with1 ~ (char(',') *> value)).repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, ","), unescape(_, ","))))
