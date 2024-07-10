package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

trait Codec[+F[+_], +A, B] extends Codec.Reader[F, A, B], Codec.Writer[F, A, B]:
  def asReader: Codec.Reader[F, A, B] = this
  def asWriter: Codec.Writer[F, A, B] = this

  def imap[C](f: B => C)(g: C => B): Codec[F, A, C]
  def default(value: B): Codec[F, A, B]
  override def optional: Codec[F, A, Option[B]]

object Codec:
  trait Reader[+F[+_], +A, +B]:
    def map[C](f: B => C): Codec.Reader[F, A, C]
    def default[B1 >: B](value: B1): Codec.Reader[F, A, B1]
    def optional: Codec.Reader[F, A, Option[B]]
    def decode(data: Option[Data.Value]): Codec.Result[B]

  trait Writer[+F[+_], +A, -B]:
    def contramap[C](f: C => B): Codec.Writer[F, A, C]
    def optional: Codec.Writer[F, A, Option[B]]
    def encode(b: B): Option[Data]

  type Result[A] = Validated[Violations[Constraint[Data], Data], A]
