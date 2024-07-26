package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import cats.data.Validated

sealed abstract class Fields[+O, A]:
  self =>

  def toChain: Chain[Field[?, ?]]

  final def product[P, B](fields: Fields[P, B]): Fields[O | P, (A, B)] = new Fields[O | P, (A, B)]:
    export self.toChain
    override def decode(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], (A, B))] =
      self.decode(data) match
        case Validated.Valid((data, a))    => fields.decode(data).map(_.tupleLeft(a))
        case Validated.Invalid(violations) => fields.decode(data).fold(violations.combine, _ => violations).invalid
    override def encode(ab: (A, B)): Chain[(String, O | P)] = self.encode(ab._1) ++ fields.encode(ab._2)

  def decode(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)]

  def encode(a: A): Chain[(String, O)]

object Fields:
  val Empty: Fields[Nothing, Unit] = new Fields[Nothing, Unit]:
    override def toChain: Chain[Nothing] = Chain.empty
    override def decode(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], Unit)] =
      (data, ()).valid
    override def encode(a: Unit): Chain[Nothing] = Chain.empty

  def apply[O <: Data, A](field: Field[O, A]): Fields[O, A] = new Fields[O, A]:
    override def toChain: Chain[Field[?, ?]] = Chain.one(field)
    override def decode(data: Chain[(String, Data)]): Codec.Result[(Chain[(String, Data)], A)] =
      val (head, remainders) = data.findWithRemainders { case (name, data) if name === field.name => data }
      field.decode(head.getOrElse(Data.Null)).leftMap(field.name /: _).tupleLeft(remainders)
    override def encode(a: A): Chain[(String, O)] = Chain.one(field.name -> field.encode(a))
