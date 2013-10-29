package pattern
import util.Random

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
  def generator(lo: Float, hi: Float, rnd: Random): Float = {
    var low = lo
    var high = hi

    if (lo > hi) {high = lo; low = high}

    val scale = scala.math.abs(high-low)
    low + (rnd.nextFloat()*scale)
  }

  def apply(lo: Float, hi: Float, rnd: Random= new Random()) : P0[Float] = {
    val func = ( () =>  generator(lo, hi, rnd) )
    Pattern(func)
  }

  def apply() : P3[Float, Float, Random, Float] = {
    val func = generator _
    Pattern(func)
  }
}

object BrownNoise {
   private class BrownNoiseGenerator {
    var currentValue: Option[Float] = None


    def generator(lo: Float, hi: Float, rnd: Random): Float = {
      if (currentValue.isEmpty) {
        currentValue = Some((lo+hi)/2)
      }

      var low = lo
      var high = hi

      if (lo > hi) {high = lo; low = high}

      // integrated white noise
      val brownNoise = (currentValue.get + rnd.nextFloat()) / 2;
      currentValue = Some(brownNoise);

      val scale = scala.math.abs(high-low)
      low + (brownNoise*scale)
    }

  }
  def apply(lo: Float, hi: Float, rnd: Random= new Random()) : P0[Float] = {
    val bGen = new BrownNoiseGenerator
    val func = ( () =>  bGen.generator(lo, hi, rnd) )
    Pattern(func)
  }

  def apply() : P3[Float, Float, Random, Float] = {
    val bGen = new BrownNoiseGenerator
    val func = bGen.generator _
    Pattern(func)
  }
}
