package pattern
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

import clock.{FakeClock, Clock}

class Context(val clock: Clock) {
  protected case class ContextState(stretchFactor: Double = 1.0)
  private var transformationStack : List[ContextState] = List();

  var currentState = ContextState(1.0)

  private var inval = false;
  def invalid = { inval }
  def invalidate = { inval = true }

  def pushState() = {
    transformationStack = currentState :: transformationStack
    currentState = ContextState(1.0)
  }

  def popState() = {
    currentState = transformationStack.head
    transformationStack = transformationStack.tail
  }

  val time = new {
    val currentMusicalTime = clock.currentMusicalTime
     var duration = MusicalDuration(0.5)

    def stretch(factor: Double) = {
      currentState = currentState.copy(stretchFactor = currentState.stretchFactor*factor)
    }

    def every[T](time: MusicalDuration)(block: => T): Option[T] = {
      if (MusicalDuration.happensRepeatedly(currentMusicalTime, time, clock.accuracy)) {
        val ret = block
        updateDuration(time)

        Some(ret)

      } else {
        None
      }
    }

    /*
    once etc sind alle stateful...
        def duration[T](time: MusicalDuration)(block: => T): T = {
      val ret = block

      updateDuration(time)

      ret
    }

    def in(time: MusicalDuration)(block: => ()) : Unit = {
      // STATEFUL IN PATTERN? nope nicht wiederverwendbar
        if(MusicalDuration.happensRepeatedly(currentMusicalTime, time, clock.accuracy)) {
          block
          updateDuration(time)

      }
    }
    */



    // global minimum for updates
    private def updateDuration(dur: MusicalDuration) = {
      val currentDuration = MusicalDuration(currentState.stretchFactor*dur.toDouble)
      this.duration = MusicalDuration.min(this.duration, currentDuration)
    }
  }

}

class DefaultContext extends Context(FakeClock)


object Context {

  def defaultContext = new DefaultContext

  def apply(clock: Clock) = new Context(clock)

}