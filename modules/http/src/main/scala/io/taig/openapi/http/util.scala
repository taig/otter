package io.taig.openapi.http

import cats.syntax.all.*

import java.nio.charset.{Charset, IllegalCharsetNameException, UnsupportedCharsetException}

final private[http] case class ContentType(mediaType: String, charset: Option[Charset])

private[http] def parseContentType(value: String): Option[ContentType] =
  value.split(";\\s*").toList match
    case mediaType :: tail =>
      val charset = tail
        .map(_.split("\\s*=\\s*", 2))
        .collectFirst { case Array(name, value) if name.equalsIgnoreCase("charset") => value }
        .map(charset => if charset.startsWith("\"") && charset.endsWith("\"") then charset.tail.init else charset)
        .flatMap: charset =>
          try Charset.forName(charset).some
          catch {
            case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
          }
      ContentType(mediaType, charset).some
    case Nil => none
