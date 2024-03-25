package io.taig.otter

import io.taig.hmap.HMap
import io.taig.hmap.Key
import scala.annotation.targetName

abstract class Metadata[+T, A]:
  val values: A
  def set(values: A): T

object Metadata:
  extension [T, A](self: Metadata[T, HMap[A]])
    def apply[B](key: Key[B] & Singleton & A): B = self.values.apply(key)

    def apply[B](key: Key[B] & Singleton & A, b: B): T = self.set(self.values.put(key, b))

    @targetName("set")
    def apply[B](key: Key[Option[B]] & Singleton & A, b: B): T = apply(key, Some(b))

    def clear[B](key: Key[Option[B]] & Singleton & A): T = apply(key, None)

    def update[B](key: Key[B] & Singleton & A)(f: B => B): T = self.set(self.values.update(key)(f))

  final case class Annotation[S, M](self: S, metadata: Metadata[Annotation[S, M], M])

  def apply[T, A](initial: A)(f: A => T): Metadata[T, A] = new Metadata[T, A]:
    override val values: A = initial
    override def set(values: A): T = f(values)
