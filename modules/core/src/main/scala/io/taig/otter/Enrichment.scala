package io.taig.otter

import cats.Traverse
import cats.derived.*
import cats.syntax.all.*

final case class Enrichment[+A](self: A, metadata: Metadata) derives Traverse:
  def map[B](f: A => B): Enrichment[B] = copy(self = f(self))

  def modifyMetadata(f: Metadata => Metadata): Enrichment[A] = copy(metadata = f(metadata))

object Enrichment:
  def apply[A](self: A): Enrichment[A] = Enrichment(self, metadata = Metadata.Empty)

  def liftK[S[_]]: [A] => S[A] => Enrichment[S[A]] = [A] => (self: S[A]) => Enrichment(self)

  def unliftK[S[_]]: [A] => Enrichment[S[A]] => S[A] = [A] => (value: Enrichment[S[A]]) => value.self
