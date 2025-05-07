package io.taig.otter.http

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import cats.parse.Parser
import cats.parse.Parser.*
import cats.syntax.all.*
import java.nio.charset.StandardCharsets
import cats.parse.Parser0
import io.taig.otter.Violation
import org.typelevel.ci.*
import java.nio.charset.Charset

final class FormDataPayloadDecoder extends PayloadDecoder[FormData]:
  override def apply[A](codec: FormData[A], contentType: MediaType, bytes: Array[Byte]): Validated[Violations, A] =
    val charset = contentType.parameters
      .get(ci"charset")
      .headOption
      .flatMap(value =>
        try Charset.forName(value).some
        catch { case _: IllegalArgumentException => none }
      )
      .getOrElse(StandardCharsets.UTF_8)

    val value = new String(bytes, charset)

    FormDataPayloadDecoder.parser
      .parseAll(value)
      .toValidated
      .leftMap: error =>
        Violations.rootNec(Violation.tpe(name = "x-www-url-formencoded", actual = value, hint = error.show))
      .andThen(data => FormDataDecoder(codec, data))

object FormDataPayloadDecoder:
  private val parser: Parser0[List[(String, Option[String])]] =
    val reserved = Set(' ', '=', '&')
    val token = charWhere(value => !reserved.contains_(value)).rep.string
    (token ~ (char('=') *> token).?).repSep0(char('&'))

  val Default: PayloadDecoder[FormData] = new FormDataPayloadDecoder()
