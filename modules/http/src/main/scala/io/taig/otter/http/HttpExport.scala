package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Enriched

trait HttpExport:
  type Body[+S[_], A] = Enriched[Self.Body[S, *], A]

  type Result[+S[_], A] = Enriched[Self.Result[S, *], A]
  
  type Results[+S[_], A] = Enriched[Self.Results[S, *], A]

  type Response[+S[_], A] = Enriched[Self.Response[S, *], A]

object HttpExport extends HttpExport
