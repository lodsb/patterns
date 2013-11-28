package pattern.Noise

import util.Random
import pattern.{Pattern, P2}

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

object BrownNoise {
  private class BrownNoiseGenerator {
    var currentValue: Option[Double] = None


    def generator(lo: Double, hi: Double, rnd: Random): Double = {
      if (currentValue.isEmpty) {
        currentValue = Some((lo+hi)/2)
      }

      var low = lo
      var high = hi

      if (lo > hi) {high = lo; low = high}

      // integrated white noise
      val brownNoise = (currentValue.get + rnd.nextDouble()) / 2;
      currentValue = Some(brownNoise);

      val scale = scala.math.abs(high-low)
      low + (brownNoise*scale)
    }

  }

  /*
  def apply(lo: Double, hi: Double, rnd: Random= new Random()) : P0[Double] = {
    val bGen = new BrownNoiseGenerator
    val func = ( () =>  bGen.generator(lo, hi, rnd) )
    Pattern(func)
  }
  */

  def apply() : P2[Double, Double, Double] = {
    val bGen = new BrownNoiseGenerator
    val random = new Random()
    val func = (lo: Double, hi: Double) => bGen.generator(lo, hi, random)
    Pattern(func)
  }
}
