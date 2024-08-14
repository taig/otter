package io.taig.otter.http.csv

opaque type Row = List[Cell]

object Row:
  extension (self: Row) inline def toList: List[Cell] = self

  def apply(cells: List[Cell]): Row = cells
