package io.taig.otter

import io.taig.otter.validation.Violation
import cats.data.Validated

abstract class Value[+O, A] extends Codec[O, A]:
  override def imap[B](f: A => B)(g: B => A): Value[O, B]
  override def optional: Value[O, Option[A]]
  override def update(f: Metadata => Metadata): Value[O, A]

  def parse(value: String): Validated[Violation[String, String], A]

  def print(a: A): String

object Value:
  abstract class Required[+O, A] extends Value[O, A]:
    override def imap[B](f: A => B)(g: B => A): Value.Required[O, B]
    override def optional: Value.Required[O, Option[A]]
    override def update(f: Metadata => Metadata): Value.Required[O, A]
