version       = "0.2.12"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.4.0"
requires "hdrawing >= 0.1.2", "hnimast >= 0.3.10"
requires "hmisc >= 0.10.0"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]
