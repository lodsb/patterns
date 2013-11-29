package pattern.time

import pattern._

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

object Every {
  def apply[T]() : P2[MusicalDuration, T, T] = {
    val func = { (ctx: Context, mt: MusicalDuration , t: T) => {
      ctx.time.block("EveryDayWeSingHappySongs").every(mt){
       None
      }

      t
    }
  }
    new P2(func)
  }
}

object In {
  def apply[T]() : P2[MusicalDuration, T, T] = {
    val func = { (ctx: Context, mt: MusicalDuration , t: T) => {
      ctx.time.block("SometimesSadOnes").in(mt){
        None
      }
    }
      t
    }
    new P2(func)
  }
}

object During {
  def apply[T]() : P2[MusicalDuration, T, T] = {
    val func = { (ctx: Context, mt: MusicalDuration , t: T) => {
      ctx.time.block("ButMostlyIListenToLadyGaga").during(mt){
        None
      }
    }
      t
    }
    new P2(func)
  }
}

object Inc {
  def apply() : P2[MusicalDuration, Int, Int] = {
    val func = { (ctx: Context, mt: MusicalDuration , wrapAt: Int) => {
      ctx.time.block("AndNicoMuhly").inc(mt)
    }
    }
    new P2(func)
  }
}

object Dec {
  def apply() : P2[MusicalDuration, Int, Int] = {
    val func = { (ctx: Context, mt: MusicalDuration , wrapAt: Int) => {
      ctx.time.block("AlsoMrSubotnick").dec(mt)
    }
    }
    new P2(func)
  }
}
