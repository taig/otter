package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import cats.data.Validated
import cats.Id as Identity

sealed abstract class Fields[+O <: Data, A]:
  self =>

  def toChain: Chain[Field[?, ?]]

  final def product[P <: Data, B](fields: Fields[P, B]): Fields[O | P, (A, B)] = new Fields[O | P, (A, B)]:
    export self.toChain
    override def decodeRecord(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], (A, B))] =
      self.decodeRecord(data) match
        case Validated.Valid((data, a)) => fields.decodeRecord(data).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          fields.decodeRecord(data).fold(violations.combine, _ => violations).invalid
    override def decodeArray(data: Vector[Data]): Codec.Result[(Vector[Data], (A, B))] = ???
    override def encodeRecord(ab: (A, B)): Chain[(String, O | P)] =
      self.encodeRecord(ab._1) ++ fields.encodeRecord(ab._2)
    override def encodeArray(ab: (A, B)): Vector[O | P] = self.encodeArray(ab._1) ++ fields.encodeArray(ab._2)

  final def toTuple: Tuple[Identity, O, A] = ???

  def decodeRecord(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)]

  def decodeArray(data: Vector[Data]): Codec.Result[(Vector[Data], A)]

  def encodeRecord(a: A): Chain[(String, O)]

  def encodeArray(a: A): Vector[O]

object Fields:
  val Empty: Fields[Nothing, Unit] = new Fields[Nothing, Unit]:
    override def toChain: Chain[Nothing] = Chain.empty
    override def decodeRecord(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], Unit)] =
      (data, ()).valid
    override def decodeArray(data: Vector[Data]): Codec.Result[(Vector[Data], Unit)] = (data, ()).valid
    override def encodeRecord(a: Unit): Chain[Nothing] = Chain.empty
    override def encodeArray(a: Unit): Vector[Nothing] = Vector.empty

  def apply[O <: Data, A](field: Field[O, A]): Fields[O, A] = new Fields[O, A]:
    override def toChain: Chain[Field[?, ?]] = Chain.one(field)
    override def decodeRecord(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)] =
      val (head, remainders) = data.findWithRemainders { case (name, data) if name === field.name => data }
      field.decode(head.getOrElse(Data.Null)).leftMap(field.name /: _).tupleLeft(remainders)
    override def decodeArray(data: Vector[Data]): Codec.Result[(Vector[Data], A)] = data.uncons match
      case Some((head, tail)) => field.decode(head).tupleLeft(tail)
      case None               => ???
    override def encodeRecord(a: A): Chain[(String, O)] = Chain.one(field.name -> field.encode(a))
    override def encodeArray(a: A): Vector[O] = Vector(field.encode(a))
