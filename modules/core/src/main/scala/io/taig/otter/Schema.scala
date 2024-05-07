package io.taig.otter

import io.taig.otter.Schema.Write
import io.taig.otter.validation.Validation

trait Schema[+Of, A] extends Schema.Read[Of, A], Schema.Write[Of, A]:
  def asRead: Schema.Read[Of, A]
  def asWrite: Schema.Write[Of, A]
  def ivalidate[B, C](constraint: Schema.Write.Any[?, B])(validation: Validation[A, B, C])(f: C => A): Schema[Of, C]
  def optional: Schema[Of, Option[A]]

object Schema:
  type Any[+Of, A] = Collection[Of, A] // | Primitive[A]

  trait Read[+Of, +A]:
    def validate[B, C](constraint: Schema.Write.Any[?, B])(validation: Validation[A, B, C]): Schema.Read[Of, C]
    def optional: Schema.Read[Of, Option[A]]

  object Read:
    type Any[+Of, +A] = Collection.Read[Of, A]

  trait Write[+Of, -A]:
    def contramap[B](f: B => A): Schema.Write[Of, B]
    def optional: Schema.Write[Of, Option[A]]

  object Write:
    type Any[+Of, -A] = Collection.Write[Of, A]
