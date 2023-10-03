package io.taig.otter.http

import cats.Eq

final case class MediaType(tpe: MediaType.Type, subtype: MediaType.Subtype):
  def print: String = s"$tpe/$subtype"
  override def toString: String = print

object MediaType:
  opaque type Type = String

  object Type:
    extension (self: MediaType.Type)
      def /(subtype: MediaType.Subtype): MediaType = MediaType(self, subtype)
      def toString: String = self

    def apply(value: String): MediaType.Type = value

    val * : MediaType.Type = "*"
    val application: MediaType.Type = "application"
    val text: MediaType.Type = "text"

    given (using eq: Eq[String]): Eq[MediaType.Type] = eq

  opaque type Subtype = String

  object Subtype:
    extension (self: MediaType.Subtype) def toString: String = self

    def apply(value: String): MediaType.Subtype = value

    val * : MediaType.Subtype = "*"
    val json: MediaType.Subtype = "json"
    val plain: MediaType.Subtype = "plain"
    val octetStream: MediaType.Subtype = "octet-stream"

    given (using eq: Eq[String]): Eq[MediaType.Subtype] = eq

  def parse(value: String): Option[MediaType] = value.split('/') match
    case Array(tpe, subtype) if tpe.nonEmpty && subtype.nonEmpty => Some(Type(tpe) / Subtype(subtype))
    case _                                                       => None

  object application:
    def apply(subtype: MediaType.Subtype): MediaType = Type.application / subtype
    val json: MediaType = application(Subtype.json)
    val octetStream: MediaType = application(Subtype.octetStream)

  object text:
    def apply(subtype: MediaType.Subtype): MediaType = Type.text / subtype
    val plain: MediaType = text(Subtype.plain)
