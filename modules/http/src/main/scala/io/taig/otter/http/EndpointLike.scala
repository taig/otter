package io.taig.otter.http

import cats.data.Chain

trait EndpointLike[A]:
  def description: Option[String]
  def description(f: Option[String] => Option[String]): A
  final def description(value: Option[String]): A = description(_ => value)
  final def description(value: String): A = description(Some(value))

  def hidden: Boolean
  def hidden(f: Boolean => Boolean): A
  final def hidden(value: Boolean): A = hidden(_ => value)

  def operationId: Option[String]
  def operationId(f: Option[String] => Option[String]): A
  final def operationId(value: Option[String]): A = operationId(_ => value)
  final def operationId(value: String): A = operationId(Some(value))

  def summary: Option[String]
  def summary(f: Option[String] => Option[String]): A
  final def summary(value: Option[String]): A = summary(_ => value)
  final def summary(value: String): A = summary(Some(value))

  def tags: Chain[String]
  def tags(f: Chain[String] => Chain[String]): A
  final def tags(values: Chain[String]): A = tags(_ => values)
  final def tags(values: String*): A = tags(Chain.fromSeq(values))
