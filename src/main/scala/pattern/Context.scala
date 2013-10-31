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

//should have some state stuff for functions -> LSystems etc.
// pattern + player + thunk hashed?
// okay.. every block gets its own dedicated state "space"

class Context(val clock: Clock, val player: BasePlayer[_], val oldContext: Option[Context] = None) {

  case class PatternStateKey(hash: Long, name: String)

  case class PatternStateEntry[T](value: T)

  type StateMap = Map[PatternStateKey, PatternStateEntry[_]]

  protected case class ContextState(stretchFactor: Double = 1.0)

  private var transformationStack: List[ContextState] = List();

  var currentState = ContextState(1.0)

  private var inval = false;

  def invalid = {
    inval
  }

  def invalidate = {
    inval = true
  }

  // this is all hardcore ugly...
  private var currentPattern: Option[BasePattern] = None

  def setCurrentPattern(pattern: BasePattern) = {
    currentPattern = Some(pattern)
  }

  class PatternState(oldState: Option[StateMap] = None) {

    var stateMap = oldState.getOrElse(Map[PatternStateKey, PatternStateEntry[_]]())


    private def apply[T](name: String, defaultValue: T, hash: Long): T = {
      val state = stateMap.get(PatternStateKey(hash, name))

      var ret = defaultValue

      if (state.isDefined) {
        val unpack = state.get
        ret = unpack match {
          case PatternStateEntry(x: T) => x
          case _ => defaultValue
        }
      }
      ret
    }

    private def save[T](name: String, value: T, hash: Long): Unit = {
      stateMap = stateMap + (PatternStateKey(hash, name) -> PatternStateEntry(value))
    }

    def apply[T](name: String, defaultValue: T): T = {
      val hash = genCurrentContextualHashCode
      apply(name, defaultValue, hash)
    }

    def save[T](name: String, value: T): Unit = {
      val hash = genCurrentContextualHashCode
      save(name, value, hash)
    }

    def apply[T](block: TimeBlock, name: String, defaultValue: T): T = {
      val hash = genCurrentContextualHashCode
      apply(block.name+name, defaultValue, hash)
    }

    def save[T](block: TimeBlock, name: String, value: T): Unit = {
      val hash = genCurrentContextualHashCode
      save(block.name+name, value, hash)
    }
  }

  var oldContexState: Option[StateMap] = None
  if (oldContext.isDefined) {
    oldContexState = Some(oldContext.get.state.stateMap)
  }

  val state = new PatternState(oldContexState)

  case class TimeBlock(val name: String, private var tbState: ContextState = ContextState(1.0)) {
    //TODO runtime exception if used several times
    def every[T](mtime: MusicalDuration)(block: => T): Option[T] = {
      if (MusicalDuration.happensRepeatedly(time.currentMusicalTime, mtime, clock.accuracy)) {
        val ret = block
        tbUpdateTime(mtime)

        Some(ret)

      } else {
        None
      }
    }

    def block(name: String) = new TimeBlock(this.name+"#HAHA#"+Context.hashCode()+"#HAHA#"+name,tbState)

    def during[T](mtime: MusicalDuration, name: String = "")(block: => T): Option[T] = {
      val durTSt = "durationTime"+name
      val timeUESt="timeOut"+name

      val durTime = state(this,durTSt, mtime)

      // duration has changed
      if (durTime != mtime) {
        state.save(this, durTSt, mtime)
        state.save(this, timeUESt, time.currentMusicalTime+mtime)
      }

      val timeOut = state(this, timeUESt, time.currentMusicalTime + mtime)

      val endInterval = time.currentMusicalTime - timeOut
      tbUpdateTime(endInterval)

      if (timeOut <= time.currentMusicalTime){
       Some(block)
      } else {
        None
      }
    }

    def in[T](mtime: MusicalDuration, name: String="")(block: => T): Option[T] = {
      val d = during(mtime, "in_dur"+name){ true }

      if (d.isEmpty) {
        Some(block)
      } else {
        None
      }
    }

    private def tbUpdateTime(dur: MusicalTime) {
      val currentDuration = MusicalDuration(currentState.stretchFactor * dur.toDouble)
      time.updateDuration(currentDuration)
    }

    def stretch(factor: Double) = {
      tbState = currentState.copy(stretchFactor = tbState.stretchFactor * factor)
    }
  }

  private def genCurrentContextualHashCode: Long = {
    val playerHash = player.hashCode().toLong
    val patternHash = currentPattern.map(x => x.hashCode().toLong).getOrElse(-1L)

    (playerHash * Int.MaxValue) + patternHash
  }

  def pushState() = {
    transformationStack = currentState :: transformationStack
    currentState = ContextState(1.0)
  }

  def popState() = {
    currentState = transformationStack.head
    transformationStack = transformationStack.tail
  }

  val time = new {
    def block(name: String) = new TimeBlock(name)

    val currentMusicalTime = clock.currentMusicalTime
    var duration = MusicalDuration(0.5)

    def stretch(factor: Double) = {
      currentState = currentState.copy(stretchFactor = currentState.stretchFactor * factor)
    }

    // global minimum for updates
    protected[Context] def updateDuration(dur: MusicalTime) = {
      val currentDuration = MusicalDuration(currentState.stretchFactor * dur.toDouble)
      this.duration = MusicalDuration.min(this.duration, currentDuration)
    }
  }

}

class DefaultContext extends Context(FakeClock, FakePlayer)


object Context {

  def defaultContext = new DefaultContext

  def apply(clock: Clock, player: Player[_]) = new Context(clock, player)
  def apply(clock: Clock, player: Player[_], oldContext: Option[Context]) = new Context(clock, player, oldContext)

}
