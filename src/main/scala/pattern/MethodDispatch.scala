package pattern

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2013-10-30 :: 00:13
    >>  Origin: ${PROJECT_NAME}
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


    Based on post by Aaron Novstrup
    @ http://stackoverflow.com/questions/8580560/
    how-to-call-the-correct-method-in-scala-java-based-the-types-of-two-objects-with
 */
class MethodDispatch[A, R] {
  private var dispatch: List[PartialFunction[A, R]] = Nil

  def += (func: PartialFunction[A, R]) = {
    dispatch +:= func
  }

  def -= (func: PartialFunction[A, R]) = {
    dispatch = dispatch.filter(_ != func)
  }


  def clear() = {
    dispatch = Nil
  }

  def apply(args: A): Unit = {
    dispatch filter  {
      _.isDefinedAt(args)
    } map {
      _.apply(args)
    }
  }
}
