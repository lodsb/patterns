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

object SyncedLFO {
  //speed, waveform (sin, ramp, triangle, square, noise?) , phase
  // phase between 0, 1
  private val steps = 64 // TODO: Should be adaptive, always smooth

  object Waveform extends Enumeration {
    val Saw = Value("SAW")
    val Square = Value("SQR")
    val Sine = Value("SIN")
    val Triangle = Value("TRI")
  }

  def generator(ctx: Context, speed: MusicalDuration, phase: Double, waveform: Waveform.Value) : Double = {
    // unit phase
    val incFactor = (1.0/steps)
    val phasor : Double= ( ctx.time.block("phase").inc(speed.stretch(incFactor), wrapAt = steps) ) * incFactor

    val currentPhase = ( phasor+(steps.toDouble/phase) ) % 1.0

    val ret = waveform match {
        case Waveform.Saw => currentPhase

        case Waveform.Square => {
            if (currentPhase >= 0.5)
              1.0
            else
              0.0
        }
        case Waveform.Triangle => 123.0

        case Waveform.Sine => scala.math.sin( currentPhase * 2 * scala.math.Pi )

        case _ => 0.0
    }




    0.1

  }
}

// deterministic random in time?
