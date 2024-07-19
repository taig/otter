package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.Keys.*
import cats.Invariant

sealed abstract class Field[+O, A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encodeValue, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(values: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], B)] =
      self.decode(values).map(_.map(f))
    override def encodeValue(b: B): Data = self.encodeValue(g(b))

  final def :*[P, B](field: Field[P, B]): Record[O & P, (A, B)] = toRecord.product(field.toRecord)
  
  final def *:[P, B](field: Field[P, B]): Record[P & O, (B ,A)] = field.toRecord.product(toRecord)

  final def toRecord: Record[O, A] = Record(this)

  def decode(values: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)]

  final def encode(a: A): Option[(String, Data)] = (metadata(nulls).getOrElse(Null.Default), encodeValue(a)) match
    case (Null.Hide, Data.Null) => None
    case (_, data)              => (name, data).some

  protected def encodeValue(a: A): Data

object Field:
  def apply[A](name: String, codec: Codec[?, A]): Field[codec.type, A] =
    val _name = name
    val _codec = codec

    new Field[codec.type, A]:
      override def name: String = _name
      override def codec: Codec[?, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(values: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)] =
        val (head, remainders) = values.findWithRemainders { case (reference, data) if reference === name => data }
        codec.decode(head.getOrElse(Data.Null)).leftMap(name /: _).tupleLeft(remainders)
      override def encodeValue(a: A): Data = codec.encode(a)

  given [O]: Invariant[Field[O, *]] with
    override def imap[A, B](fa: Field[O, A])(f: A => B)(g: B => A): Field[O, B] = fa.imap(f)(g)
