package io.taig.otter.http

final class UrlOps[A](self: Url[A]):
  inline def zip[B](url: Url[B]): Url[(A, B)] = self.product(url)
  def zip(url: Url[Unit]): Url[A] = self.product(url).imap { case (a, _) => a }((_, ()))
  inline def /[B](segment: Segment[B]): Url[(A, B)] = self.zip(segment.toPath.toUrl)
  def /(segment: Segment[Unit]): Url[A] = self.zip(segment.toPath.toUrl)
  def /(static: String): Url[A] = /(Segment.Static(static))
  def :?[B](query: Query[B]): Url[(A, B)] = zip(query.toQueries.toUrl)

final class UrlOpsUnit(self: Url[Unit]):
  def zip[B](url: Url[B]): Url[B] = self.product(url).imap { case (_, b) => b }(((), _))
  def /[A](segment: Segment[A]): Url[A] = self.zip(segment.toPath.toUrl)
  def /(static: String): Url[Unit] = /(Segment.Static(static))
  def :?[A](query: Query[A]): Url[A] = zip(query.toQueries.toUrl)

trait ToUrlOps extends ToUrlOps1:
  implicit final def toUrlOpsUnit(self: Url[Unit]): UrlOpsUnit = UrlOpsUnit(self)
trait ToUrlOps1:
  implicit final def toUrlOps[A](self: Url[A]): UrlOps[A] = UrlOps(self)
