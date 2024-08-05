package io.taig.otter.http

import cats.Eq
import cats.syntax.all.*
import cats.data.Chain

import java.nio.charset.Charset

final case class MediaType(tpe: MediaType.Type, parameters: MediaType.Parameters):
  def print: String = if parameters.isEmpty then tpe.print else s"${tpe.print}; ${parameters.print}"
  override def toString: String = print

object MediaType:
  final case class Type(primary: MediaType.Type.Primary, secondary: MediaType.Type.Secondary):
    def print: String = s"$primary/$secondary"
    override def toString: String = print

  object Type:
    opaque type Primary = String

    object Primary:
      extension (self: MediaType.Type.Primary)
        def /(subtype: MediaType.Type.Secondary): MediaType =
          MediaType(MediaType.Type(self, subtype), Parameters.Empty)

      def apply(value: String): MediaType.Type.Primary = value

      val * : MediaType.Type.Primary = "*"
      val application: MediaType.Type.Primary = "application"
      val text: MediaType.Type.Primary = "text"

      given (using eq: Eq[String]): Eq[MediaType.Type.Primary] = eq

    opaque type Secondary = String

    object Secondary:
      def apply(value: String): MediaType.Type.Secondary = value

      val * : MediaType.Type.Secondary = "*"
      val json: MediaType.Type.Secondary = "json"
      val plain: MediaType.Type.Secondary = "plain"
      val octetStream: MediaType.Type.Secondary = "octet-stream"

      given (using eq: Eq[String]): Eq[MediaType.Type.Secondary] = eq

    def parse(value: String): Option[MediaType.Type] = value.split('/') match
      case Array(primary, secondary) if primary.nonEmpty && secondary.nonEmpty => Some(Type(primary, secondary))
      case _                                                                   => None

    given Eq[MediaType.Type] = (x, y) => x.primary === y.primary && x.secondary === y.secondary

  opaque type Parameters = Chain[(String, String)]

  object Parameters:
    extension (self: MediaType.Parameters)
      def toChain: Chain[(String, String)] = self
      def isEmpty: Boolean = toChain.isEmpty
      def charset: Option[Charset] = toChain
        .collectFirstSome { case (key, value) => Option.when(key.equalsIgnoreCase("charset"))(value) }
        .flatMap: value =>
          try Some(Charset.forName(value))
          catch case _: IllegalArgumentException => None
      def print: String = toChain.map { case (key, value) => s"$key=$value" }.mkString_("; ")

    val Empty: MediaType.Parameters = Chain.empty

    def fromChain(value: Chain[(String, String)]): MediaType.Parameters = value
    def apply(values: (String, String)*): MediaType.Parameters = fromChain(Chain.fromSeq(values))

    def parse(value: String): Option[MediaType.Parameters] = Chain
      .fromIterableOnce(value.split(';'))
      .traverse: value =>
        value.split("=", 2) match
          case Array(key, value) => Some(key.trim -> value.trim)
          case _                 => None

    given (using eq: Eq[Chain[(String, String)]]): Eq[MediaType.Parameters] = eq

  def parse(value: String): Option[MediaType] = value.split(";", 2) match
    case Array(mediaType)             => Type.parse(mediaType).map(MediaType(_, Parameters.Empty))
    case Array(mediaType, parameters) => (Type.parse(mediaType), Parameters.parse(parameters)).mapN(MediaType.apply)
    case _                            => None

  object application:
    def apply(subtype: MediaType.Type.Secondary): MediaType = Type.Primary.application / subtype
    val json: MediaType = application(Type.Secondary.json)
    val octetStream: MediaType = application(Type.Secondary.octetStream)

  object text:
    def apply(subtype: MediaType.Type.Secondary): MediaType = Type.Primary.text / subtype
    val plain: MediaType = text(Type.Secondary.plain)

  given Eq[MediaType] = Eq.by(mediaType => (mediaType.tpe, mediaType.parameters))
