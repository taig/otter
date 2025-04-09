package io.taig.otter

import cats.~>
import cats.implicits.*

sealed abstract class Dictionary[+S[_], +T[_], A]:
  def metadata: Metadata
  def key: Reference[S, ?]
  def value: Reference[T, ?]
  def constraints: Vector[Constraint.Object]
  def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, A]
  def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, A]
  def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, A]
  final def imap[B](f: A => B)(g: B => A): Dictionary[S, T, B] = Dictionary.Modify(self = this, f, g)

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
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, List[(A, B)]] = copy(value = value.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, List[(A, B)]] = copy(key = key.mapK(fK))

  final private[otter] case class Modify[S[_], T[_], A, B](self: Dictionary[S, T, A], f: A => B, g: B => A)
      extends Dictionary[S, T, B]:
    export self.{constraints, key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Dictionary[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: T1 ~> U): Dictionary[S, U, B] = copy(self = self.mapK(fK))
    override def leftMapK[S1[a] >: S[a], U[_]](fK: S1 ~> U): Dictionary[U, T, B] = copy(self = self.leftMapK(fK))
