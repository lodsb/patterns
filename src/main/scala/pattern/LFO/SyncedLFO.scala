package pattern.lfo

import pattern.{Pattern, Context, MusicalDuration}
import scala.util.Random

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

object SyncedLFO {
  //speed, waveform (sin, ramp, triangle, square, noise?) , phase
  // phase between 0, 1
  private val steps = 128 // TODO: Should be adaptive, always smooth
  private val duration = MusicalDuration(1).d128

  object Waveform extends Enumeration {
    val Saw = Value("SAW")
    val Square = Value("SQR")
    val Sine = Value("SIN")
    val Triangle = Value("TRI")
    val RandomSH = Value("TRI")
  }

  //TODO: Reset
  def generator(ctx: Context, speed: MusicalDuration, phase: Double, waveform: Waveform.Value, reset: Boolean) : Double = {
    val timeFactor = duration.toDouble/speed.toDouble

    //fixed speed phasor
    //unit phase
    val phasor : Double= ( ctx.time.block("phase").inc(duration, wrapAt = steps) ) / steps.toDouble

    // update current phase
    var currentPhase : Double = ctx.state("currentPhase", 0.0)

    if (!reset) {
      currentPhase = (currentPhase + timeFactor) % 1.0
    } else {
      currentPhase = 0.0;
    }

    ctx.state.save("currentPhase", currentPhase)

    currentPhase = currentPhase + phase

    val ret = waveform match {
      case Waveform.Saw => 1-currentPhase

      case Waveform.Square => {
        if (currentPhase >= 0.5)
          1.0
        else
          0.0
      }
      case Waveform.Triangle => {
        if (currentPhase >= 0.5) {
          1-(2*currentPhase)
        } else {
          2*currentPhase
        }
      }

      case Waveform.RandomSH => {
        // created only if needed... kinda disgusting
        val random: Random = ctx.state("randomGenerator", new Random())
        val oldPhaseDirection: Double = ctx.state("phaseDirection", math.signum(currentPhase))
        val oldRandomValue: Double = ctx.state("oldRandomValue", 0)

        val phaseDirection = math.signum(currentPhase)

        val randomValue = if (phaseDirection != oldPhaseDirection) {
          random.nextDouble()
        } else {
          oldRandomValue
        }

        ctx.state.save("oldRandomValue", randomValue)
        ctx.state.save("oldPhaseDirection", phaseDirection)
        ctx.state.save("randomGenerator", random)


        randomValue
      }

      case Waveform.Sine => scala.math.sin( currentPhase * 2 * scala.math.Pi )

      case _ => 0.0
    }

    ret

  }

  def apply() = Pattern(generator _)
}

