import hpprint, hdrawing, strformat
import hpprint/hpprint_repr
import strutils
export hdrawing

proc pprintSizes*[T](obj: T, sizes: seq[int]): void =
  var buf: seq[StrBlock]
  for size in sizes:
    buf.add newTermGridVert(@[
      &"max: {size}",
      pstring(obj, maxWidth = size)
    ]).toStringBlock()

  let shape = newTermGridHoriz(buf, ' ')
  echo &"input: {obj}"
  echo shape.toStringBlock().join("\n")

proc pprintTrees*[T](obj: T): void =
  var buf: seq[StrBlock]

  buf.add newTermGridVert(@[
    "obj tree",
    pstring(obj, maxWidth = 20)
  ]).toStringBlock()

  buf.add newTermGridVert(@[
    "tree repr",
    obj.toObjTree().treeRepr()
  ]).toStringBlock()

  buf.add newTermGridVert(@[
    "lisp repr",
    obj.toObjTree().lispRepr(newlines = true)
  ]).toStringBlock()

  let shape = newTermGridHoriz(buf, ' ')
  echo shape.toStringBlock().join("\n")

proc pprintConfigs*[T](obj: T): void =
  let jsonConf = PPrintConf(
    maxWidth: 30,
    identStr: "  ",
    seqSeparator: ", ",
    seqPrefix: "",
    seqWrapper: (makeDelim("["), makeDelim("]")),
    objWrapper: (makeDelim("{"), makeDelim("}")),
    fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
    fldSeparator: ",",
    kvSeparator: ": ",
    alignFieldsRight: true
  )


  let lispConf = PPrintConf(
    maxWidth: 30,
    identStr: "  ",
    seqSeparator: "",
    seqPrefix: "",
    seqWrapper: (makeDelim("'("), makeDelim(")")),
    objWrapper: (makeDelim("("), makeDelim(")")),
    fldNameWrapper: (makeDelim(""), makeDelim("")),
    fldSeparator: "",
    kvSeparator: ": ",
    alignFieldsRight: true
  )

  var buf: seq[StrBlock]

  buf.add newTermGridVert(@[
    "Regular output",
    obj.pstring(maxWidth = 30)
  ]).toStringBlock()

  buf.add newTermGridVert(@[
    "Json-like output",
    obj.pstring(jsonConf)
  ]).toStringBlock()

  buf.add newTermGridVert(@[
    "LISP-like output",
    obj.pstring(lispConf)
  ]).toStringBlock()

  let shape = newTermGridHoriz(buf, ' ')
  echo shape.toStringBlock().join("\n")

when isMainModule:
  pprintSizes(@[1, 2, 3], @[2, 10])

  pprintSizes(@[@[1,2,], @[3,4]], @[2, 10, 20])

  pprintSizes((a: 2, b: 33), @[4, 30])

  type
    UserType = object
      field1: seq[int]
      field2: string
      subnodes: seq[UserType]

  let val = UserType(
    field1: @[1,2,3],
    field2: "hello",
    subnodes: @[
      UserType(field1: @[2,8,3,4,54])
    ]
  )

  pprintTrees(val)
  pprintConfigs(val)
