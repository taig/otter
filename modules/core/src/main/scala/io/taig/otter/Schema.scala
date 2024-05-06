package io.taig.otter

import io.taig.otter.Schema.Write

trait Schema[+Of, A] extends Schema.Read[Of, A], Schema.Write[Of, A]:
  def asRead: Schema.Read[Of, A]
  def asWrite: Schema.Write[Of, A]
  def imap[B](f: A => B)(g: B => A): Schema[Of, B]
  def optional: Schema[Of, Option[A]]

object Schema:
  type Any[+Of, A] = Collection[Of, A]

  trait Read[+Of, +A]:
    def map[B](f: A => B): Schema.Read[Of, B]
    def optional: Schema.Read[Of, Option[A]]

  object Read:
    type Any[+Of, +A] = Collection.Read[Of, A]

  trait Write[+Of, -A]:
    def contramap[B](f: B => A): Schema.Write[Of, B]
    def optional: Schema.Write[Of, Option[A]]

  object Write:
    type Any[+Of, -A] = Collection.Write[Of, A]
