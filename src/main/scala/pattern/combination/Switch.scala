package pattern.combination

import pattern.{Context, P0, P2}

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

object Switch {
  def apply[T](updateAll: Boolean = false): P2[Seq[P0[T]], Int, T] = {
    val func = (ctx: Context, seq: Seq[P0[T]], idx: Int) => {
      val ss = seq.size-1
      val ri = if (idx > 0) {
        idx % ss
      } else {
        ss - (idx % ss)
      }

      if (updateAll) {
        seq.zipWithIndex.foreach({ x =>
          if(x._2 != ri) {
            x._1(ctx)
          }
        })
      }

      val p = seq(ri)
      p(ctx)

    }

    new P2(func)
  }
}