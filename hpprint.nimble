version       = "0.2.10"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.4.0"
requires "shell", "with"
requires "hdrawing", "hasts", "hnimast >= 0.3.10"
requires "hmisc >= 0.9.15"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]
