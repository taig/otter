package io.taig.otter

sealed abstract class Dictionary[+S[_], +T[_], A] extends Codec[T, A]:
  def key: Reference[S, ?]
  def value: Reference[T, ?]
  def constraints: Vector[Constraint.Object]
  override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, A]
  final override def imap[B](f: A => B)(g: B => A): Dictionary[S, T, B] = Dictionary.Modify(self = this, f, g)

object Dictionary:
  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference[S, A],
      value: Reference[T, B],
      minimum: Option[Int],
      maximum: Option[Int],
      metadata: Metadata
  ) extends Dictionary[S, T, List[(A, B)]]:
    override def constraints: Vector[Constraint.Object] = Vector(
      minimum.map(Constraint.Object.MinProperties.apply),
      maximum.map(Constraint.Object.MaxProperties.apply)
    ).flatten
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, List[(A, B)]] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Dictionary[S, T, A], f: A => B, g: B => A)
      extends Dictionary[S, T, B]:
    export self.{constraints, key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, B] = copy(self = self.modifyMetadata(f))

  given [S[_], T[_]]: CodecInvariant[Dictionary[S, T, *]] with
    override def imap[A, B](fa: Dictionary[S, T, A])(f: A => B)(g: B => A): Dictionary[S, T, B] = fa.imap(f)(g)
