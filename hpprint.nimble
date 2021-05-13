version       = "0.2.18"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.4.0"
requires "hdrawing >= 0.1.3", "hnimast >= 0.3.18"
requires "hmisc >= 0.10.1"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]
