package io.taig.otter.http

import cats.Eq
import cats.Show
import io.taig.enumeration.ext.Mapping

enum Method:
  case Delete
  case Get
  case Head
  case Options
  case Patch
  case Post
  case Put
  case Trace

  final override def toString: String = Method.mapping(this)

object Method:
  given mapping: Mapping[Method, String] = Mapping.enumeration:
    case Delete  => "DELETE"
    case Get     => "GET"
    case Head    => "HEAD"
    case Options => "OPTIONS"
    case Patch   => "PATCH"
    case Post    => "POST"
    case Put     => "PUT"
    case Trace   => "TRACE"

  given Eq[Method] = Eq.fromUniversalEquals

  given Show[Method] = mapping.apply(_)
