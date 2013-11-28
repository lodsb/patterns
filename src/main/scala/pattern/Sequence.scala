import javax.swing.plaf.metal.MetalBorders.OptionDialogBorder
import pattern._
import scala.Some
import pattern.util._
import pattern.util.Done
import scala.Some
import util.Cont
import util.Done
import util.Element

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


/*
abstract class Generating[ContainerType, DataType] {
  // takes a container, generates a value from it, None if finished
  def generate(container: ContainerType): Option[DataType]

  def reset()
}*/

trait Generating[ContainerType, DataType] extends Enumerator[DataType] {
  def reset
}

abstract class Interpolating[DataType, ReturnType] {
  def iteratee(): Iteratee[DataType, Option[ReturnType]]
}
   /*
abstract class Interpolating[DataType, ReturnType] {
  def needASample(): Boolean

  def feedSample(sample: Option[DataType])

  def interpolate(): Option[ReturnType]
}    */

// using composition, so they can changed at runtime
class Sequence[Input,Intermediate,Ret](generating: Generating[Input, Intermediate],
                                       interpolating: Interpolating[Intermediate,Ret])
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


      val int = interpolator().iteratee()
      val gen = generator()

      val ret = gen.enum(int)


      ret match {
        case Done(result, _) => result match {
          case Some(s) => Some(s)
          case _ => None
        }
        // Something went wrong
        case _ => None
      }
  })
}

// implementations interpolation

class NoInterpolation[A] extends Interpolating[A,A] {
  def iteratee(): Iteratee[A, Option[A]] = {
    def step(in: Input[A]) : Iteratee[A, Option[A]]= {
      in match {
        case EOF => Done(None, EOF)
        case Empty=> Cont(step)
        case Element(x) => Done(Some(x), Empty)
      }
    }

    Cont(step)
  }
}

class Mean[A <% Float](numberOfValues: Int) extends Interpolating[A,A] {
  def iteratee(): Iteratee[A, Option[A]] = {
    def step(akk: Int, sum: Float)(in: Input[A]) : Iteratee[A, Option[A]]= {
      in match {
        case EOF => Done(None, EOF)
        case Empty=> Cont(step(akk, sum))
        case Element(x) => { if (akk == 0) {
                              // arggglll
                              Done(Some((x+sum)/numberOfValues).asInstanceOf[Option[A]], Empty)
                          } else {
                              Cont(step(akk-1, x+sum))
                          }
        }
      }
    }

    if (numberOfValues == 0) {
      Done(None, EOF)
    } else {
      Cont(step(numberOfValues, 0f))
    }
  }
}

// TODO LERP ETC stretchfactor < 1, drop samples + interpol; > 1 interpolate



//TODO: Sample & Hold


// implementations generator

class Repetition[A](container: P0[A], repetitions: P0[Int]) extends Generating[P0[A], A] {
  private var counter = 0;
  private var reps = 0;

  private var currentValue : Option[A] = None

  def generate(): A = {
    val currentRepetitions = repetitions();

    if (reps != currentRepetitions) {
      reps = currentRepetitions;

      reset();
    }

    if (counter <= currentRepetitions) {
      counter = counter + 1;
    } else {
        reset();
    }

    if (currentValue.isEmpty) {
      currentValue = Some(container())
    }

    currentValue.get
  }

  def reset() {
    counter = 0;
    currentValue = None
  }

  def enum[Out](iter: Iteratee[A, Out]): Iteratee[A, Out] = {
    val feed = generate()

    def step(i: Iteratee[A, Out]) : Iteratee[A, Out] = {
      iter match {
        case Done(_,_) => iter
        case c@Cont(_) => step(c(Element(feed)))
      }
    }

    step(iter)
  }
}

class WrappedSequence[A](seq: P0[Seq[A]], deviation: P0[Int]) extends Generating[P0[A], A] {
  private var currentIndex = 0;

  def reset {
    currentIndex = 0
  }

  def enum[Out](iter: Iteratee[A, Out]): Iteratee[A, Out] = {
    val currentSequence = seq()

    def step(i: Iteratee[A, Out]) : Iteratee[A, Out] = {
      iter match {
        case Done(_,_) => iter
        case c@Cont(_) => {
          val dev = deviation()

          val seqSize = currentSequence.size-1
          var index = currentIndex % seqSize

          if (index < 0) {
            index = seqSize + currentIndex
          }

          val elem = currentSequence(currentIndex % (currentSequence.size-1))

          currentIndex = currentIndex + dev

          step(c(Element(elem)))
        }

      }
    }

    step(iter)
  }
}


/** *
  * companion object containing
  * convenience functions for quick instantiation
  */
object Sequence {
  def apply[T](pseq: P0[Seq[T]]) : Sequence[Seq[T],T,T] = {
    import Implicits._
    import Pattern._
    val deviator = Pattern(1)

    new Sequence(new WrappedSequence[T](pseq, deviator), new NoInterpolation[T])
  }
}


