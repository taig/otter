package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Types.*
import io.taig.otter.findWithRemainders
import org.typelevel.ci.CIString
import cats.data.Chain
import io.taig.otter.validation.Violations
import io.taig.otter.validation.History
import io.taig.otter.validation.Violation
import io.taig.otter.Data

sealed abstract class Header[A]:
  def name: CIString

  def codec: Value.Required[?] | Collection.Of[Value.Required[?], ?]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Header[A] = ???

  final def imap[B](f: A => B)(g: B => A): Header[B] = ???

  def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)]

  def encode(a: A): Http.Headers

object Header:
  def apply[A](name: CIString, codec: Value.Required[A]): Header[A] =
    val _name = name
    val _codec = codec

    new Header[A]:
      override def name: CIString = _name
      override def codec: Value.Required[A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)] =
        val (value, remainders) = headers.findWithRemainders { case (`_name`, value) => value }
        codec.parse(value).tupleLeft(remainders)
      override def encode(a: A): Http.Headers = Chain.one((name, codec.printValue(a)))

  def apply[A](name: CIString, codec: Collection.Of[Value.Required[?], A]): Header[A] =
    val _name = name
    val _codec = codec

    new Header[A]:
      override def name: CIString = _name
      override def codec: Collection.Of[Value.Required[?], A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)] = ???
      override def encode(a: A): Http.Headers = ??? // Chain.fromSeq(codec.printValue(a)).tupleLeft(name)
