version       = "0.2.9"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.2.6"
requires "shell", "with"
requires "hdrawing", "hasts", "hnimast"
requires "hmisc >= 0.8.0"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]
