package pattern.noise

import scala.util.Random
import pattern.{P1, Pattern, P2}

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

object Coin {

    def generator(prob: Double, rnd: Random): Boolean = {
      // have to recheck, dont think this is exact
      val noise = rnd.nextDouble()

      noise >= prob

  }

  def apply() : P1[Double, Boolean] = {
    val random = new Random()
    val func = (prob: Double) => generator(prob, random)
    Pattern(func)
  }
}
