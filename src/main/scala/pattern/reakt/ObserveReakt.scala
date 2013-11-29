package pattern.reakt

import pattern._
import org.lodsb.reakt.TSignal


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

// TODO: loopback that leads to a re-evaluation of the dataflow graph of patterns
// re-eval flag?
object ObserveReakt {
  def apply[T]() : P2[TSignal[T], MusicalDuration,T] = {
    val func = (ctx: Context, sig: TSignal[T], mt: MusicalDuration) => {
      val value = sig()

      // define minimum sampling rate
      ctx.time.block("ObserveReakt").every(mt){
        None
      }
      sig()
    }

    new P2(func)
  }
}
