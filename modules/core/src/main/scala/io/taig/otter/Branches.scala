package io.taig.otter

import cats.data.NonEmptyVector
import cats.syntax.all.*
import cats.Id as Identity

sealed abstract class Branches[+O <: Data, A]:
  self =>

  def toNev: NonEmptyVector[Branch[?, ?]]

  final def imap[B](f: A => B)(g: B => A): Branches[O, B] = new Branches[O, B]:
    export self.toNev
    override def decodeNested(
        data: Vector[(String, Data)],
        discriminator: Discriminator.Nested
    ): Codec.Result[Option[B]] =
      self.decodeNested(data, discriminator).map(_.map(f))
    override def decodeMerged(
        data: Vector[(String, Data)],
        discriminator: Discriminator.Merged
    ): Codec.Result[Option[B]] =
      self.decodeMerged(data, discriminator).map(_.map(f))
    override def decodeKeyed(data: Vector[(String, Data)]): Codec.Result[Option[B]] =
      self.decodeKeyed(data).map(_.map(f))
    override def decodeUntagged(data: Data): Codec.Result[Option[B]] = self.decodeUntagged(data).map(_.map(f))
    override def encodeNested(b: B, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
      self.encodeNested(g(b), discriminator)
    override def encodeMerged[P <: Data](b: B, discriminator: Discriminator.Merged)(using
        O <:< Data.Object[P]
    ): Data.Object[Data.String | P] = self.encodeMerged(g(b), discriminator)
    override def encodeKeyed(b: B): Data.Object[O] = self.encodeKeyed(g(b))
    override def encodeUntagged(b: B): O = self.encodeUntagged(g(b))

  final def orElse[P <: Data, B](branches: Branches[P, B]): Branches[O | P, Either[A, B]] =
    new Branches[O | P, Either[A, B]]:
      override def toNev: NonEmptyVector[Branch[?, ?]] = self.toNev.concatNev(branches.toNev)
      override def decodeNested(
          data: Vector[(String, Data)],
          discriminator: Discriminator.Nested
      ): Codec.Result[Option[Either[A, B]]] = ???
      override def decodeMerged(
          data: Vector[(String, Data)],
          discriminator: Discriminator.Merged
      ): Codec.Result[Option[Either[A, B]]] = ???
      override def decodeKeyed(data: Vector[(String, Data)]): Codec.Result[Option[Either[A, B]]] = ???
      override def decodeUntagged(data: Data): Codec.Result[Option[Either[A, B]]] = ???
      override def encodeNested(
          ab: Either[A, B],
          discriminator: Discriminator.Nested
      ): Data.Object[Data.String | (O | P)] =
        ab.fold(self.encodeNested(_, discriminator), branches.encodeNested(_, discriminator))
      override def encodeMerged[Q <: Data](ab: Either[A, B], discriminator: Discriminator.Merged)(using
          (O | P) <:< Data.Object[Q]
      ): Data.Object[Data.String | Q] =
        ab.fold(self.encodeMerged(_, discriminator), branches.encodeMerged(_, discriminator))
      override def encodeKeyed(ab: Either[A, B]): Data.Object[O | P] = ab.fold(self.encodeKeyed, branches.encodeKeyed)
      override def encodeUntagged(ab: Either[A, B]): O | P = ab.fold(self.encodeUntagged, branches.encodeUntagged)

  final def :+[P <: Data, B](branch: Branch[P, B]): Branches[O | P, Either[A, B]] = orElse(branch.toBranches)

  final def +:[P <: Data, B](branch: Branch[P, B]): Branches[P | O, Either[B, A]] = branch.toBranches.orElse(this)

  final def toSumNested: Sum.Nested[Identity, O, A] = Sum.Nested(this)
  final def toSumMerged[P <: Data](using
      evidence: this.type <:< Branches[Data.Object[P], A]
  ): Sum.Merged[Identity, P, A] = Sum.Merged(evidence(this))
  final def toSumKeyed: Sum.Keyed[Identity, O, A] = Sum.Keyed(this)
  final def toSumUntagged: Sum.Untagged[Identity, O, A] = Sum.Untagged(this)

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
    override def decodeUntagged(data: Data): Codec.Result[Option[A]] = branch.decode(data).toOption.valid
    override def encodeNested(a: A, discriminator: Discriminator.Nested): Data.Object[Data.String | O] =
      Data.Object.one(discriminator.identifier, Data.String(branch.name)) ++ Data.Object.fromOption(
        Some(discriminator.value).filter(_ =!= discriminator.identifier).tupleRight(branch.encode(a))
      )
    override def encodeMerged[P <: Data](a: A, discriminator: Discriminator.Merged)(using
        O <:< Data.Object[P]
    ): Data.Object[Data.String | P] = Data.Object.one(discriminator.identifier, Data.String(branch.name)) ++
      branch.encode(a).filterKeys(_ =!= discriminator.identifier)
    override def encodeKeyed(a: A): Data.Object[O] = Data.Object.one(branch.name, branch.encode(a))
    override def encodeUntagged(a: A): O = branch.encode(a)
