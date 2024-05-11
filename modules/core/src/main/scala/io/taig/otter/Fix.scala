package io.taig.otter

// Whatcha gonna do, call type police?
opaque type Fix[+S[+_]] = S[Any]

object Fix:
  extension [S[+_]](self: Fix[S]) def unfix: S[Fix[S]] = self.asInstanceOf

  def apply[S[+_]](value: S[Fix[S]]): Fix[S] = value

  def unapply[S[+_]](self: Fix[S]): Some[S[Fix[S]]] = Some(self.unfix)
