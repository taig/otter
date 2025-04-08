package io.taig.otter.http

import io.taig.otter.*

final class HttpHeaderPrinter(explode: Boolean) extends Printer[Http.Header]:
  val obj = HttpHeaderObjectPrinter(explode)

  override def apply[A](codec: Http.Header[A], a: A): String = codec match
    case codec: Http.Header.Array[A]  => HttpHeaderArrayPrinter(codec, a)
    case codec: Http.Header.Object[A] => obj(codec, a)
    case codec: Http.Header.Value[A]  => HttpHeaderValuePrinter(codec, a)

object HttpHeaderPrinter:
  def apply(explode: Boolean): Printer[Http.Header] = new HttpHeaderPrinter(explode)
