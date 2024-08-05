package io.taig.otter.http

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset

opaque type FormData = Vector[(String, String)]

object FormData:
  extension (self: FormData)
    def get(key: String): Vector[String] = self.collect { case (`key`, value) => value }
    def getFirst(key: String): Option[String] = self.collectFirst { case (`key`, value) => value }
    def print(charset: Charset): String =
      self
        .map { case (key, value) =>
          s"${URLEncoder.encode(key, charset)}=${URLEncoder.encode(value, StandardCharsets.UTF_8)}"
        }
        .mkString("&")

  def parse(value: String): Option[FormData] =
    ???
