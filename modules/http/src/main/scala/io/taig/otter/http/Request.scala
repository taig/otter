package io.taig.otter.http

import io.taig.otter.Schema

sealed trait Request[+Segment[+_], +Query[+_], +Header[+_], +Body[+_], +Schema[+_], +A]:
  def method: Method
  def url: Url[Segment, Query, Schema, ?]
  def headers: Headers[Header, Schema, ?]
  def body: Body[Request.Body[Schema, ?]]

object Request:
  sealed trait Body[+F[+_], +A]:
    def schema: F[Schema.Reader[F, ?, ?]]

  object Body:
    sealed trait Singlepart[+F[+_], +A] extends Request.Body[F, A]

    object Singlepart:
      final case class Strict[F[+_], A](schema: F[Schema.Reader[F, ?, A]]) extends Request.Body.Singlepart[F, A]
