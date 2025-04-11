package io.taig.otter.http

import cats.Eq
import cats.Show

enum Method:
  case Delete
  case Get
  case Head
  case Options
  case Patch
  case Post
  case Put
  case Trace

  override def toString(): String = this match
    case Delete  => "DELETE"
    case Get     => "GET"
    case Head    => "HEAD"
    case Options => "OPTIONS"
    case Patch   => "PATCH"
    case Post    => "POST"
    case Put     => "PUT"
    case Trace   => "TRACE"

object Method:
  given Eq[Method] = Eq.fromUniversalEquals

  given Show[Method] = Show.fromToString
