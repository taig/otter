package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Enrichment

trait HttpExport:
  type Body[+S[_], A] = Enrichment[Self.Body[S, *], A]

  type Result[+S[_], A] = Enrichment[Self.Result[S, *], A]

  type Results[+S[_], A] = Enrichment[Self.Results[S, *], A]

  type Response[+S[_], A] = Enrichment[Self.Response[S, *], A]

object HttpExport extends HttpExport
