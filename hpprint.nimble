version       = "0.2.12"
author        = "haxscramper"
description   = "Generic configurable pretty-printer"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.4.0"
requires "shell", "with"
requires "hdrawing >= 0.1.2", "hasts >= 0.1.4", "hnimast >= 0.3.10"
requires "hmisc >= 0.9.16"


let
  testDir = "/tmp/docker-hpprint"
  localDevel = @["hmisc"]
