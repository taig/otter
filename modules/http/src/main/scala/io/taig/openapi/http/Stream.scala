package io.taig.openapi.http

//sealed abstract class Entity[A]:
//  def isEmpty: Boolean
//  def imap[B](f: A => B)(g: B => A): Entity[B]
//
//object Entity:
//  abstract class Streaming[A] extends Entity[A]:
//    override def imap[B](f: A => B)(g: B => A): Entity.Streaming[B]
//    def consume: Entity.Strict[Array[A]]
//
//  abstract class Strict[A] extends Entity[A]:
//    override def imap[B](f: A => B)(g: B => A): Entity.Strict[B]

abstract class Stream[+A]:
  def isEmpty: Boolean

object Stream:
  val Empty: Stream[Nothing] = new Stream[Nothing]:
    override def isEmpty: Boolean = true
