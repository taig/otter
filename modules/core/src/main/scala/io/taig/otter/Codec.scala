package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

trait Codec[+F[+_], +A, B] extends Codec.Reader[F, A, B], Codec.Writer[F, A, B]:
  def imap[C](f: B => C)(g: C => B): Codec[F, A, C]
  override def optional: Codec[F, A, Option[B]]
  def default(value: Option[B]): Codec[F, A, B]

object Codec:
  trait Reader[+F[+_], +A, +B]:
    def map[C](f: B => C): Codec.Reader[F, A, C]
    def optional: Codec.Reader[F, A, Option[B]]
    def default[B1 >: B](value: Option[B1]): Codec.Reader[F, A, B1]
    def decode(data: Data): Validated[Violations[Data, Data], B]

  trait Writer[+F[+_], +A, -B]:
    def contramap[C](f: C => B): Codec.Writer[F, A, C]
    def optional: Codec.Writer[F, A, Option[B]]
    def encode(b: B): Data
