package pattern.reakt

import pattern.{Context, P0}
import org.lodsb.reakt.async._

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-10-29 :: 20:37
    >>  Origin: patterns
    >>
  +3>>
    >>  Copyright (c) 2013:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */

class SignalPattern[T](f: Context => T) extends P0[T](f) {
  // source for trouble??
  val signal = new ValA[T](this.apply())

  override def apply(ctx: Context) : T = {
    val ret = super.apply(ctx)
    signal.emit(ret)

    ret
  }
}

object SignalPattern {
  def apply[T](p: P0[T]) : SignalPattern[T]= {
    new SignalPattern[T](p.func)
  }
}
