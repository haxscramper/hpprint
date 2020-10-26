# Package

version       = "0.2.8"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"



# Dependencies

requires "nim >= 1.2.6"
requires "shell", "with"
requires "hdrawing", "hasts", "hnimast"
requires "hmisc >= 0.8.0"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]

template canImport(x: untyped): untyped =
  compiles:
    import x


when canImport(hmisc/other/nimbleutils):
  import hmisc/other/nimbleutils

  task dockertestDevel, "Test in docker container with local packages":
    runDockerTestDevel(
      AbsDir thisDir(),
      AbsDir testDir, localDevel, "nimble test") do:
        writeTestConfig("""
          --verbosity:0
          --hints:off
          --warnings:off
        """)

    rmDir testDir


  task dockertestAllDevel, "Test in docker container with local packages":
    runDockerTestDevel(
      AbsDir thisDir(),
      AbsDir testDir, localDevel, "nimble testallTask") do:
        writeTestConfig("""
          --verbosity:0
          --hints:off
          --warnings:off
        """)

  task dockertest, "Test in new docker container":
    ## Run unit tests in new docker conatiner; download all
    ## dependencies using nimble.
    runDockerTest(AbsDir thisDir(), AbsDir testDir, "nimble test") do:
      notice "Running test in docker container"

  task installtest, "Test installation from cloned repo":
    runDockerTest(AbsDir thisDir(), AbsDir testDir, "nimble install")

  task testall, "Run full test suite in all variations":
    runDockerTest(
      AbsDir thisDir(),
      AbsDir testDir,
      "nimble install hmisc@#head && nimble testallTask"
    )

  task testallTask, "~~~ testall implementation ~~~":
    testAllImpl()
