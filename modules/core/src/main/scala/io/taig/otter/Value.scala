package io.taig.otter

import io.taig.otter.Codec.Reader

trait Value[+F[+_], +A, B] extends Codec[F, A, B], Value.Reader[F, A, B], Value.Writer[F, A, B]:
  override def asReader: Value.Reader[F, A, B] = this
  override def asWriter: Value.Writer[F, A, B] = this
  override def default(value: B): Value[F, A, B]
  override def optional: Value[F, A, Option[B]]

object Value:
  trait Reader[+F[+_], +A, +B] extends Codec.Reader[F, A, B]:
    override def default[B1 >: B](value: B1): Value.Reader[F, A, B1]
    override def map[C](f: B => C): Value.Reader[F, A, C]
    override def optional: Value.Reader[F, A, Option[B]]

  trait Writer[+F[+_], +A, -B] extends Codec.Writer[F, A, B]:
    override def contramap[C](f: C => B): Value.Writer[F, A, C]
    override def optional: Value.Writer[F, A, Option[B]]
