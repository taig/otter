package io.taig.otter

trait Format[+A]:
  type Out <: Data

object Format:
  type Aux[+A, B <: Data] = Format[A] { type Out = B }
  
  def apply[A, B <: Data] = new Format[A] { override type Out = B }

  given Format.Aux[Product[?, ?], Data.Array[?]] = ???

type Format2[A] <: Data = A match
  case Primitive[?] => Data.Primitive
  case Dictionary[?, ?] => Data.Object
  case Codec[?, ?] => Data
  // case Product[?, ?] => Data.Array[?]