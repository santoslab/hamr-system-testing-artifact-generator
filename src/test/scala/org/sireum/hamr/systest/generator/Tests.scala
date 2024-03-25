package org.sireum.hamr.systest.generator

import org.sireum._
import org.sireum.hamr.systest.generator
import org.sireum.message.Reporter

class Tests extends org.sireum.test.TestSuite {

  val repoRoot: Os.Path = Os.home / "devel" / "git" / "hamr-system-testing-case-studies"

  "pca" in {
    val root: Os.Path = Os.home / "devel/git/openpcapump-855-2024/architecture-855/hamr/slang"

    println(root)

    val containers: ISZ[String] = ISZ(
      "src/main/data/PCA855/Aux_Types.scala"
    )

    val reporter = Reporter.create
    val result = SystemTestArtifactGen.run(root, containers, reporter)

    reporter.printMessages()

    assert(result == 0 && !reporter.hasError)
  }

  "rts" in {
    val root: Os.Path = repoRoot / "rts" / "hamr" / "slang"

    println(root)

    val containers: ISZ[String] = ISZ(
      "src/main/util/RTS/system_tests/rts1/Containers.scala"
    )

    val reporter = Reporter.create
    val result = generator.SystemTestArtifactGen.run(root, containers, reporter)

    reporter.printMessages()

    assert(result == 0 && !reporter.hasError)
  }

  "isolette" in {
    val root: Os.Path = repoRoot / "isolette" / "hamr" / "slang"

    println(root)

    val containers: ISZ[String] = ISZ(
      "src/main/util/isolette/system_tests/rst/Regulate_Subsystem_Containers.scala",
      "src/main/util/isolette/system_tests/monitor1/Monitor_Subsystem_Containers.scala"
    )

    val reporter = Reporter.create
    val result = generator.SystemTestArtifactGen.run(root, containers, reporter)

    reporter.printMessages()

    assert(result == 0 && !reporter.hasError)
  }

  "boiler_control" in {
    val root: Os.Path = repoRoot / "boiler_control" / "hamr" / "slang"

    println(root)

    val containers: ISZ[String] = ISZ(
      // TODO
    )

    val reporter = Reporter.create
    val result = generator.SystemTestArtifactGen.run(root, containers, reporter)

    reporter.printMessages()

    assert(result == 0 && !reporter.hasError)
  }
}
