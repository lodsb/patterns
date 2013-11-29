package pattern.combination

import pattern.{Context, P2, P0}

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

class JoinSequences {
  def apply[T]() : P2[Seq[T],Seq[T],Seq[T]]= {
    val func = (ctx: Context, s1: Seq[T], s2: Seq[T]) => {
      s1++s2
    }

    new P2(func)
  }
}
