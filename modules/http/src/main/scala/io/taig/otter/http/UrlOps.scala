package io.taig.otter.http

final class UrlOps[A](self: Url[A]):
  def /(segment: Segment[Unit]): Url[A] = self.zip(segment.toPath.toUrl).imap { case (a, _) => a }((_, ()))
  def /(static: String): Url[A] = /(Segment.Static(static))

final class UrlOpsUnit(self: Url[Unit]):
  def /[A](segment: Segment[A]): Url[A] = self.zip(segment.toPath.toUrl).imap { case (_, b) => b }(((), _))
  def /(static: String): Url[Unit] = /(Segment.Static(static))

trait ToUrlOps extends ToUrlOps1:
  implicit final def toUrlOpsUnit(self: Url[Unit]): UrlOpsUnit = UrlOpsUnit(self)
trait ToUrlOps1:
  implicit final def toUrlOps[A](self: Url[A]): UrlOps[A] = UrlOps(self)
