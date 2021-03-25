package caio.mtl

import caio.TestInstances

import cats.mtl.{ Censor, Local, Stateful }
import cats.laws.discipline.SerializableTests
import cats.mtl.laws.discipline.{CensorTests, LocalTests, StatefulTests}

class LocalStatefulTests extends TestInstances {
  import implicits.{dynamicCaioCensor, dynamicCaioLocal, dynamicCaioStateful}
  import caio.arbitrary._

  checkAllAsync("Caio") { params =>
    import params._
    StatefulTests[CaioT, C].stateful[Int]
  }

  checkAllAsync("Stateful[Caio]") { _ =>
    SerializableTests.serializable(Stateful[CaioT, C])
  }

  checkAllAsync("Caio") { params =>
    import params._
    CensorTests[CaioT, L].censor[Int, String]
  }

  checkAllAsync("Censor[Caio]") { _ =>
    SerializableTests.serializable(Censor[CaioT, L])
  }

  checkAllAsync("Caio") { params =>
    import params._
    LocalTests[CaioT, C].local[Boolean, String]
  }

  checkAllAsync("Local[Caio]") { _ =>
    SerializableTests.serializable(Local[CaioT, C])
  }
}