package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.Keys.*
import io.circe.Json
import cats.data.Chain

object RecordJsonEncoder:
  def apply[A](schema: Record[?, A], a: A): Option[Chain[(String, Json)]] =
    RecordJsonEncoder(schema, schema.metadata(nulls).getOrElse(Null.Default), a)

  def apply[A](schema: Record[?, A], nulls: Null, a: A): Option[Chain[(String, Json)]] = schema match
    case Record.Combine(_, left, right) =>
      (RecordJsonEncoder(left, nulls, a._1).orEmpty ++ RecordJsonEncoder(right, nulls, a._2).orEmpty).some
    case Record.Empty(_)              => Chain.empty.some
    case Record.One(_, field)         => FieldJsonEncoder(field, nulls, a).map(Chain.one)
    case Record.Optional(self)        => a.flatMap(RecordJsonEncoder(self, nulls, _))
    case Record.Transform(self, _, f) => RecordJsonEncoder(self, nulls, f(a))
