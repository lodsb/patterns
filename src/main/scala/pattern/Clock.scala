package clock

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

import pattern._


abstract class Clock {
  def defaultMusicalDuration : MusicalDuration
  def accuracy : MusicalTime
  def currentMusicalTime : MusicalTime
  def start() : Unit
  def kill() : Unit
  def pause() : Unit
  def schedulePlayer(player: BasePlayer[_]) : Unit


}

class AppClock extends Clock {

  private var scheduled = Map[Double, Set[BasePlayer[_]]]()
  private val monitor = new Object

  private var bpm = 120f

  private def clockTempoDurationMillis: Long = (bpm2Millis(bpm)*accuracy.toDouble).toLong

  protected def bpm2Millis(bpm: Float): Float = {
    60000.0f / bpm
  }

  private var running = false;

  def accuracy = MusicalDuration(1).d128 // == clock resolution

  def defaultMusicalDuration = MusicalDuration(0.5)

  private var _currentMusicalTime = new MusicalTime(1)

  // logical time
  def currentMusicalTime = _currentMusicalTime

  private def nextBeat = 1-(_currentMusicalTime.toDouble % 1.0)

  private val runningThread = new Thread(new Runnable {
    def run() {
      while (running) {
        try {
          Thread.sleep(clockTempoDurationMillis)
          _currentMusicalTime = _currentMusicalTime + accuracy

          clock(accuracy.toDouble)
        } catch {
          case _ : Throwable => running = false
        }
      }
    }
  })

  override def start() = {
    running = true
    runningThread.start()
  }

  override def kill() = {
    pause()
    monitor.synchronized {
      scheduled = Map()
    }
  }

  override def pause() = {
    running = false
    runningThread.interrupt()
  }

  //TODO: something to synchronize clocks, reset?

  override def schedulePlayer(player: BasePlayer[_]) {
    // TODO: schedule on next beat
    this.schedule(player, nextBeat)
  }

  private def schedule(player: BasePlayer[_], duration: Double) = {

    monitor.synchronized {

      val s = scheduled.get(duration)
      if (!s.isEmpty) {
        var set = s.get

        // dont permit double triggers
        if (!set.contains(player)) {
          set = set + player
          scheduled = scheduled + (duration -> set)
        }

      } else {
        scheduled = scheduled + (duration -> Set(player))
      }
    }


  }

  private def clock(passedTime: Double) = {
    scheduled = scheduled.map({
      x => (x._1 - passedTime, x._2)
    })

    var done = false
    while (!done) {
      monitor.synchronized {
        val currentMin = scheduled.minBy(_._1)

        if (currentMin._1 <= accuracy.toDouble) {
          scheduled = scheduled - currentMin._1

          val set = currentMin._2

          // could be dispatched to different threads... or every player
          // has own thread/actor??
          val nextSet = set.map({x =>
            val ctx = x.next(); (ctx, x)
          }).filter(! _._1.invalid)

          //schedule new set
          nextSet.foreach({ x =>
            if(!x._1.invalid) {
              schedule(x._2, x._1.time.duration.toDouble)
            }
          })

        } else {
          done = true
        }
      }
    }

  }
}


class FakeClock extends Clock{
  def defaultMusicalDuration = MusicalDuration(0.5)
  def currentMusicalTime = new MusicalTime(0)
  def accuracy = new MusicalTime(0.1)
  def start() {}

  def kill() {}

  def pause() {}

  def schedulePlayer(player: BasePlayer[_]) {}
}

object FakeClock extends FakeClock

