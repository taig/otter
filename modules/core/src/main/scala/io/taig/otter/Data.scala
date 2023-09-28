package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*
import cats.Eq

import java.lang.String as JString
import scala.Boolean as SBoolean
import scala.Product as SProduct

sealed abstract class Data extends SProduct with Serializable:
  final def asValue: Option[Data.Value] = this match
    case data: Data.Value => Some(data)
    case Data.Null        => None

  final def asObject: Option[Data.Object] = this match
    case data: Data.Object => Some(data)
    case _                 => None

  final def name: String = this match
    case _: Data.String  => "string"
    case _: Data.Boolean => "boolean"
    case _: Data.Number  => "number"
    case _: Data.Object  => "object"
    case _: Data.Array   => "array"
    case Data.Null       => "null"

object Data:
  sealed abstract class Value extends Data

  final case class Object(values: Chain[(JString, Data)]) extends Data.Value:
    def first(key: JString): Option[Data] = values.collectFirst { case (k, v) if key === k => v }
    def removeFirst(key: JString): Data.Object =
      var removed = false
      val result = List.newBuilder[(JString, Data)]
      values.iterator.foreach {
        case (reference, _) if key == reference && !removed => removed = true; ()
        case entry                                          => result += entry
      }
      Object(Chain.fromSeq(result.result()))
    def firstWithRemainders(key: JString): Option[(Data, Data.Object)] = first(key).tupleRight(removeFirst(key))
    def ++(obj: Data.Object): Data.Object = Object(values ++ obj.values)
  object Object:
    val Empty: Data.Object = Object(Chain.empty)
    def one(key: JString, value: Data): Data.Object = Object(Chain.one((key, value)))
    def of(kv: (JString, Data)*): Data.Object = Object(Chain.fromSeq(kv))
  final case class Array(values: Chain[Data]) extends Data.Value:
    def length: Long = values.length
    def ++(data: Data.Array): Data.Array = Array(values ++ data.values)
  object Array:
    val Empty: Data.Array = Array(Chain.empty)
    def fill(n: Long)(value: => Data): Data.Array = Array(Chain.fromSeq(Seq.fill(n.toInt)(value)))

  sealed abstract class Primitive extends Value

  final case class String(value: JString) extends Data.Primitive
  final case class Boolean(value: SBoolean) extends Data.Primitive
  final case class Number(value: Int | Long | Float | Double | BigDecimal | BigInt) extends Data.Primitive

  case object Null extends Data

  given Eq[Data] = Eq.fromUniversalEquals
