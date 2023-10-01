package io.taig.otter.http

import cats.data.Chain

abstract class Endpoint[I, O]:
  def request: Request[I]
  def request[T](f: Request[I] => Request[T]): Endpoint[T, O]
  final def request[T](value: Request[T]): Endpoint[T, O] = request(_ => value)

  def response: Response[O]
  def response[T](f: Response[O] => Response[T]): Endpoint[I, T]
  final def response[T](value: Response[T]): Endpoint[I, T] = response(_ => value)

  def deprecated: Boolean
  def deprecated(f: Boolean => Boolean): Endpoint[I, O]
  final def deprecated(value: Boolean): Endpoint[I, O] = deprecated(_ => value)

  def description: Option[String]
  def description(f: Option[String] => Option[String]): Endpoint[I, O]
  final def description(value: Option[String]): Endpoint[I, O] = description(_ => value)
  final def description(value: String): Endpoint[I, O] = description(Some(value))

  def hidden: Boolean
  def hidden(f: Boolean => Boolean): Endpoint[I, O]
  final def hidden(value: Boolean): Endpoint[I, O] = hidden(_ => value)

  def operationId: Option[String]
  def operationId(f: Option[String] => Option[String]): Endpoint[I, O]
  final def operationId(value: Option[String]): Endpoint[I, O] = operationId(_ => value)
  final def operationId(value: String): Endpoint[I, O] = operationId(Some(value))

  def summary: Option[String]
  def summary(f: Option[String] => Option[String]): Endpoint[I, O]
  final def summary(value: Option[String]): Endpoint[I, O] = summary(_ => value)
  final def summary(value: String): Endpoint[I, O] = summary(Some(value))

  def tags: Chain[String]
  def tags(f: Chain[String] => Chain[String]): Endpoint[I, O]
  final def tags(values: Chain[String]): Endpoint[I, O] = tags(_ => values)
  final def tags(values: String*): Endpoint[I, O] = tags(Chain.fromSeq(values))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] = ???
