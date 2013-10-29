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


abstract class Generating[ContainerType, DataType] {
  // takes a container, generates a value from it, None if finished
  def generate(container: ContainerType): Option[DataType]

  def reset()
}

abstract class Interpolating[DataType, ReturnType] {
  def needASample(): Boolean

  def feedSample(sample: Option[DataType])

  def interpolate(): Option[ReturnType]
}

// using composition, so they can changed at runtime
class Sequence[Input,Intermediate,Ret](interpolating: Interpolating[Intermediate,Ret],
                     generating: Generating[Input, Intermediate])
  extends P1[Input, Option[Ret]]({
    (ctx: Context, x: Input) => None
  }) {

  protected class ComponentWrapper[T](private var component: T) {
    private val monitor = new Object()

    def apply(): T = {
      monitor.synchronized {
        component
      }
    }

    def update(newComponent: T) = {
      monitor.synchronized {
        this.component = newComponent
      }
    }
  }

  val interpolator = new ComponentWrapper[Interpolating[Intermediate,Ret]](interpolating)
  val generator = new ComponentWrapper[Generating[Input, Intermediate]](generating)

  // overwrite initial function
  this.func = ({
    (ctx: Context, x: Input) =>
      while (interpolator().needASample()) {
        interpolator().feedSample(generator().generate(x))
      }

      interpolator().interpolate()
  })
}

// implementations interpolation

class NoInterpolation[A] extends Interpolating[A,A] {
  private var sampleNeeded = true
  private var currentSample: Option[A] = None

  def needASample(): Boolean = {
    val ret = sampleNeeded
    sampleNeeded = sampleNeeded ^ true

    ret
  }

  def feedSample(sample: Option[A]) = {
    currentSample = sample
  }

  def interpolate(): Option[A] = {
    currentSample
  }
}

/* TODO LERP ETC stretchfactor < 1, drop samples + interpol; > 1 interpolate
intermediate ist float, der rest kann  alles andere sein
class LinearInterpolation[T <% Float](stretchFactor: P0[Float]) extends Interpolating[(T,T),T] {
  private var sampleNeeded = true
  private var currentSample: Option[(T,T)] = None
  private var currentStretchFactor: Option[Float] = None


  def needASample(): Boolean = {
    val ret = sampleNeeded
    sampleNeeded = sampleNeeded ^ true

    ret
  }

  def feedSample(sample: Option[(T,T)]) = {
    currentSample = sample
  }

  def interpolate(): Option[Float] = {
    if (!currentSample.isEmpty) {
      val x: T = currentSample.get._1
      val y: T = currentSample.get._2
       Some((x+y)/2f)
    } else {
      None
    }
  }
}
*/

//TODO: Sample & Hold

// implementations generator
class Repetition[A](repetitions: P0[Int]) extends Generating[P0[A], A] {
  private var counter = 0;
  private var reps = 0;

  def generate(container: P0[A]): Option[A] = {
    val currentRepetitions = repetitions();
    if (reps != currentRepetitions) { reps = currentRepetitions; counter = 0;}

    if (counter < currentRepetitions) {
      counter = counter + 1;
        Some(container())
    } else {
        reset(); None
    }

  }

  def reset() {
    counter = 0;
  }
}

