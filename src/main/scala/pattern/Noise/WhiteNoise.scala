package pattern.Noise

import scala.util.Random

import pattern.Implicits
import Implicits._
import pattern.P2
import pattern.Pattern

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

object WhiteNoise {
  def generator(lo: Double, hi: Double, rnd: Random): Double = {
    var low = lo
    var high = hi

    if (lo > hi) {high = lo; low = high}

    val scale = scala.math.abs(high-low)
    low + (rnd.nextDouble()*scale)
  }

  /*
  def apply(lo: Double, hi: Double, rnd: Random= new Random()) : P0[Double] = {
    val func = ( () =>  generator(lo, hi, rnd) )
    Pattern(func)
  }
  */

  def apply() : P2[Double, Double, Double] = {
    val random = new Random
    val func = (lo: Double, hi: Double) => generator(lo, hi, random)
    Pattern(func)
  }
}
