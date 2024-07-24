package io.taig.otter

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.Keys.*
import cats.Invariant

sealed abstract class Field[+O <: Data[?], A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Field[O, A] = new Field[O, A]:
    export self.{codec, decode, encodeValue, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Field[O, B] = new Field[O, B]:
    export self.{codec, metadata, name}
    override def decode(values: Chain[(String, Data[?])]): Codec.Result[(Chain[(String, Data[?])], B)] =
      self.decode(values).map(_.map(f))
    override def encodeValue(b: B): O = self.encodeValue(g(b))

  // final def :*[P, B](field: Field[P, B]): Record[O & P, (A, B)] = toRecord.product(field.toRecord)

  // final def *:[P, B](field: Field[P, B]): Record[P & O, (B, A)] = field.toRecord.product(toRecord)

  // final def toRecord: Record[O, A] = Record(this)

  def decode(values: Chain[(String, Data[?])]): Codec.Result[(Chain[(String, Data[?])], A)]

  final def encode(a: A): Option[(String, O)] = (metadata(nulls).getOrElse(Null.Default), encodeValue(a)) match
    case (Null.Hide, Data.Null) => None
    case (_, data)              => (name, data).some

  protected def encodeValue(a: A): O

object Field:
  def apply[O <: Data[?], A](identifier: String, of: Codec[O, A]): Field[O, A] = new Field[O, A]:
    override def name: String = identifier
    override def codec: Codec[?, A] = of
    override def metadata: Metadata = Metadata.Empty
    override def decode(values: Chain[(String, Data[?])]): Codec.Result[(Chain[(String, Data[?])], A)] =
      val (head, remainders) = values.findWithRemainders { case (reference, data) if reference === name => data }
      codec.decode(head.getOrElse(Data.Null)).leftMap(name /: _).tupleLeft(remainders)
    override def encodeValue(a: A): O = of.encode(a)

  given [O <: Data[?]]: Invariant[Field[O, *]] with
    override def imap[A, B](fa: Field[O, A])(f: A => B)(g: B => A): Field[O, B] = fa.imap(f)(g)
