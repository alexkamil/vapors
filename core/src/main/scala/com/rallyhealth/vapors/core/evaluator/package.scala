package com.rallyhealth.vapors.core

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg

package object evaluator {
  import cats.instances.function._

  def eval[O, A](origin: O)(exp: FreeApplicative[ExpAlg[O, O, *], A]): A = {
    evalAtStep((origin, origin))(exp)
  }

  def evalAtStep[O, T, A](step: (O, T))(exp: FreeApplicative[ExpAlg[O, T, *], A]): A = {
    exp.foldMap(new EvalLoop[O, T]).apply(step)
  }

}
