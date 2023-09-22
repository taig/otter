//package io.taig.otter.http
//
//final class UrlOps[A](self: Url[A]):
//  def /(segment: Segment[Unit]): Url[A] = self.zip(segment.toPath.toUrl).imap { case (a, _) => a }((_, ()))
//  def /(static: String): Url[A] = /(Segment(static))
//
//trait ToUrlOps:
//  implicit final def toUrlOps[A](self: Url[A]): UrlOps[A] = UrlOps(self)
