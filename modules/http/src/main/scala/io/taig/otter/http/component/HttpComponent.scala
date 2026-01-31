package io.taig.otter.http.component

import io.taig.otter.http.Segment
import io.taig.otter.http.syntax.SegmentSyntax.*
import io.taig.otter.http.Path

trait HttpComponent:
//   object query extends QueryComponent[Http.Query, Http.Query.Parameter]:
//     object parameter extends QueryComponent.Parameter

  object segment extends SegmentComponent.Dynamic[Segment.Dynamic, Segment.Parameter]:
    object parameter extends SegmentComponent.Parameter

object HttpComponent extends HttpComponent

object Playground:
  import HttpComponent.*

  val s: Segment[String] = segment(name = "foobar", parameter = segment.parameter.string)
  toPath[Segment, Path, String](s)
  val x = s.toPath
  val y: Path[(String, String)] = s :* s
  // val q = query(name = "foobar", parameter = query.parameter.string)
  // val _ = q.toQueries
  // val _: Http.Queries[(String, String)] = q :* q
