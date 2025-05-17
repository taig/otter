package io.taig.otter.http.codec

import cats.data.Validated
import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Parser0
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import io.taig.otter.http.FormData

object FormDataPayloadDecoder extends PayloadDecoder[FormData]:
  override def decode[A](schema: FormData[A], charset: Option[Charset], bytes: Array[Byte]): Validated[Violations, A] =
    val value = new String(bytes, charset.getOrElse(StandardCharsets.UTF_8))

    FormDataPayloadDecoder.parser
      .parseAll(value)
      .toValidated
      .leftMap: error =>
        Violations.rootNec(Violation.tpe(name = "x-www-url-formencoded", actual = value, hint = error.show))
      .andThen(FormDataDecoder.decode(schema, _))

  private val parser: Parser0[List[(String, Option[String])]] =
    val reserved = Set(' ', '=', '&')
    val token = charWhere(value => !reserved.contains_(value)).rep.string
    (token ~ (char('=') *> token).?).repSep0(char('&'))
