package caio

import caio.std.{CaioParApplicative, CaioParallel}
import cats.effect.laws.discipline.ConcurrentEffectTests
import cats.laws.discipline.{CommutativeApplicativeTests, ParallelTests}

class CaioCatsEffectTests extends TestInstances {
  import arbitrary._
  import cats.effect.laws.discipline.arbitrary._

  checkAllAsync("Caio") { params =>
    import params._
    ConcurrentEffectTests[CaioT](CE, EC.contextShift[CaioT](CE)).concurrentEffect[Int, Int, Int]
  }

  checkAllAsync("Caio") { params =>
    import params._
    val CP = new CaioParallel[C, V, L]
    val module = ParallelTests[CaioT](CP)
    module.parallel[Int, Int]
  }

  checkAllAsync("ParCaio") { params =>
    import params._
    implicit val CA = new CaioParApplicative[C, V, L](CE)
    CommutativeApplicativeTests[ParCaio[C, V, L, ?]].commutativeApplicative[Int, Int, Int]
  }

  testAsync("defer evaluation until run") { CE =>
    var run = false
    val caio = Caio { run = true }
    assertEquals(run, false)
    CE.toIO(caio).unsafeRunSync()
    assertEquals(run, true)
  }
}
