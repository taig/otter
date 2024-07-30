package io.taig.otter

import cats.data.NonEmptyVector
import io.taig.otter.Codec.Result

sealed abstract class Branches[+O <: Data, A]:
  self =>

  def toNev: NonEmptyVector[Branch[?, ?]]

  final def imap[B](f: A => B)(g: B => A): Branches[O, A] = ???

  final def orElse[P <: Data, B](branches: Branches[P, B]): Branches[O | P, Either[A, B]] = ???

  final def :+[P <: Data, B](branch: Branch[P, B]): Branches[O | P, Either[A, B]] = ???

  final def +:[P <: Data, B](branch: Branch[P, B]): Branches[P | O, Either[B, A]] = ???

  def decodeNested(data: Vector[(String, Data)], discriminator: Discriminator.Nested): Codec.Result[Option[A]]

  def decodeMerged(data: Vector[(String, Data)], discriminator: Discriminator.Merged): Codec.Result[Option[A]]

  def decodeKeyed(data: Vector[(String, Data)]): Codec.Result[Option[A]]

  def decodeUntagged(data: Data): Codec.Result[Option[A]]

  def encodeNested(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O]

  def encodeMerged[P <: Data](a: A, discriminator: Discriminator.Merged)(using
      O <:< Data.Object[P]
  ): Data.Object[Data.String | P]

  def encodeKeyed(a: A): Data.Object[O]

  def encodeUntagged(a: A): O

object Branches:
  def apply[O <: Data, A](branch: Branch[O, A]): Branches[O, A] = new Branches[O, A]:
    override def toNev: NonEmptyVector[Branch[?, ?]] = NonEmptyVector.one(branch)

    override def decodeNested(
        data: Vector[(String, Data)],
        discriminator: Discriminator.Nested
    ): Codec.Result[Option[A]] = ???

    override def decodeMerged(
        data: Vector[(String, Data)],
        discriminator: Discriminator.Merged
    ): Codec.Result[Option[A]] = ???

    override def decodeKeyed(data: Vector[(String, Data)]): Codec.Result[Option[A]] = ???

    override def decodeUntagged(data: Data): Codec.Result[Option[A]] = ???

    override def encodeNested(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
      Data.Object.of(
        discriminator.value -> branch.encode(a),
        discriminator.identifier -> Data.String(branch.name)
      )

    override def encodeMerged[P <: Data](a: A, discriminator: Discriminator.Merged)(using
        O <:< Data.Object[P]
    ): Data.Object[Data.String | P] =
      branch.encode(a) ++ Data.Object.one(discriminator.identifier, Data.String(branch.name))

    override def encodeKeyed(a: A): Data.Object[O] = Data.Object.one(branch.name, branch.encode(a))

    override def encodeUntagged(a: A): O = branch.encode(a)
