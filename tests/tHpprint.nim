import unittest, sugar, sequtils, terminal

import hmisc/[helpers, hdebug_misc]
import strutils


import hmisc/hasts/[graphviz_ast, html_ast]
import hdrawing, hdrawing/block_grid

import hnimast, hnimast/obj_field_macros
import ../src/hpprint
import ../src/hpprint/[hpprint_repr]
import hmisc/types/[seq2d, htrie, colorstring]

startHax()

converter toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  makeSeq2D(s)


suite "Block grid":
  test "{makeGrid} make string grid":
    var grid = makeGrid[StrBlock](3, 3, makeThinLineGridBorders())
    for (pos, cell) in grid.itercells():
      grid[pos] = makeGrid(
        @[
          @["[||||", "world\neee"],
          @["EEEE", "####"],
          @["eee"]
        ].toStrGrid(@[""].toStrBlock()),
        makeAsciiGridBorders()
      ).toCell()

    grid.addHeader(makeCell[StrBlock](
      @["! ANNOTATION !"].toStrBlock(),
      (3, 1)
    ))
    grid.addHeader(makeCell[StrBlock](
      @["! ANNOTATION 2 !"].toStrBlock(),
      (3, 1)
    ))

  test "Block grid to html table":
    var grid = makeGrid(
      @[@["hello", "123"],
        @["world", "1234", "123"]])

    grid.setOrAdd(makeArrPos(0, 2), makeGrid(
      @[@["eee", "Ewer"], @["123", "123"]]))


suite "Block labeling":
  template testtmp(
    labels: untyped, chunkLines: seq[string] = @["[|||]"]): untyped =
    relativePosition(
      chunk = makeChunk(chunkLines), labels)

  test "Chunk label on left":
    assertEq $(testtmp(
      @{rpTopLeftLeft: (text: "<>", offset: 0)})), "<>[|||]"

  test "Chunk label on top left":
    assertEq $(testtmp(
      @{rpTopLeftAbove: (text: "<-->", offset: 2)})), "<-->\n  [|||]"

  test "Chunk label on bottom right":
    assertEq $(testtmp(
      @{rpBottomRight: (text: "<>", offset: 2)})), "[|||]<>"

  test "Chunk label on bottom left":
    assertEq $(testtmp(
      @{rpBottomLeft: (text: "<>", offset: 2)})), "  [|||]\n<>"

  test "Top-above & bottom left":
    assertEq $(testtmp(
      @{rpTopLeftAbove: (text: "{{{", offset: 2),
         rpBottomLeft: (text: "}}}", offset: 2)})),
         """
         {{{
           [|||]
         }}}""".dedent

  test "Multiline block compact":
    assertEq $(testtmp(
      @{rpBottomRight: (text: "}}", offset: 2),
         rpTopLeftLeft: (text: "{{", offset: 2)
      # Top left left offset should be ignored
    },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{[||||]
           [||||]}}""".dedent

  test "Multiline block expanded":
    assertEq $(testtmp(
      @{rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2)
      # Top left left offset should be ignored
    },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
           [||||]
           [||||]
         }}""".dedent

  test "Multiline block expanded with prefix":
    assertEq $(testtmp(
      @{rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2),
         rpPrefix: (text: "- ", offset: 2)
      # Top left left offset should be ignored
    },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
         - [||||]
         - [||||]
         }}""".dedent
  test "Multiline block with colored prefix":
    assertEq $(testtmp(
      @{rpBottomLeft: (text: "\e[31m}}\e[39m", offset: 2),
         rpTopLeftAbove: (text: "\e[31m{{\e[39m", offset: 2),
         rpPrefix: (text: "- ", offset: 2)
      # Top left left offset should be ignored
    },
      chunkLines = @["[||||]", "[||||]"]
    )),
         "\e[31m{{\e[39m\n- [||||]\n- [||||]\n\e[31m}}\e[39m"

  test "Invalid prefix assertion":
    try:
      discard testtmp(@{
        rpTopLeftLeft: (text: "==", offset: 2),
        rpPrefix: (text: "--", offset: 2)
      })

      fail("Unreachable code")
    except ArgumentError:
      assert getCurrentExceptionMsg().startsWith("Incompatible chunk labels")

    except:
      fail("Wrong exception")


type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

var conf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("(", multiline = true),
               makeDelim(")", multiline = false)),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": "
  # wrapLargerThan: 10
)

template pstr(arg: untyped, ident: int = 0): untyped =
  var conf = conf
  conf.idCounter = makeCounter()
  let tree = toSimpleTree(arg, conf, @[])
  prettyString(tree, conf, ident)

suite "Simple configuration":
  test "integer":
    assertEq pstr(12), "12"

  test "indentation":
    assertEq pstr(12, 3), "   12"

  test "string":
    assertEq pstr("112"), "\"112\""

  test "Anonymous tuple":
    assertEq pstr((12, "sdf")), "(12, \"sdf\")"

  test "Named tuple":
    assertEq pstr((a: "12", b: "222")), "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    conf.maxWidth = 6
    assertEq @[1, 2, 3, 4].pstr(), "- 1\n- 2\n- 3\n- 4"

  test "Wide sequence":
    conf.maxWidth = 80
    assertEq @[1, 2, 3].pstr(), "[1, 2, 3]"

  test "int-int table":
    assertEq {2: 3, 4: 5}.toOrderedTable().pstr(), "{2: 3, 4: 5}"

  test "Sequence of tuples":
    assertEq @[(1, 3), (4, 5)].pstr(), "[(1, 3), (4, 5)]"

  type
    T = object
      f1: int

  test "Simple object":
    assertEq T(f1: 12).pstr(), "T(f1: 12)"

  test "Multiline constant":
    type
      Tt = object
        f2: string

    let str = Tt(f2: """
      Aliquam erat volutpat. Nunc sad asdfd
      non orci commodo lobortis. Proin nequ
      lobortis eget, lacus. Sed diam. Praes
      Nullam tempus. Mauris ac felis vel ve
      pede. Etiam vel neque nec dui digniss
      Phasellus neque orci, porta a, alique
      Phasellus purus. Pellentesque tristiq""".dedent()).pstr()
    # echo str
    assertEq str, """
      Tt(
        f2:
          "Aliquam erat volutpat. Nunc sad asdfd
          non orci commodo lobortis. Proin nequ
          lobortis eget, lacus. Sed diam. Praes
          Nullam tempus. Mauris ac felis vel ve
          pede. Etiam vel neque nec dui digniss
          Phasellus neque orci, porta a, alique
          Phasellus purus. Pellentesque tristiq"""".dedent

  test "Sequence of objects":
    assertEq @[T(f1: 12), T(f1: -99)].pstr(), "[T(f1: 12), T(f1: -99)]"

  type
    C = object
      case kind: bool
      of true: f90: string
      of false: f09: (float, string)

  test "Case object":
    assertEq C(kind: true, f90: "12").pstr(), "C(kind: true, f90: \"12\")"
    assertEq C(kind: false, f09: (1.2, "12")).pstr(),
         "C(kind: false, f09: (1.2, \"12\"))"

suite "Colored printing":
  test "Field annotation":
    type
      A = object
        f1: seq[int]
        f2: seq[A]

    var tree = toObjTree(A(
      f1: @[21],
      f2: @[
      A(f1: @[1, 2, 3, 4, 5, 6]),
      A(f1: @[1, 2, 3, 4, 5, 6]),
      A(f1: @[1, 2, 3, 4, 5, 6]),
      A(f1: @[1, 2, 3, 4, 5, 6])
    ]))

    tree.getAtPath(@[objAccs("f1"), seqAccs(0)]).annotate(" Hello".toRed())
    tree.getAtPath(@[objAccs("f2")]).annotate("Hello".toGreen())
    tree.getAtPath(@[objAccs("f2")]).stylize(initStyle(fgRed))

    tree.getAtPath(@[objAccs("f2"), seqAccs(1)]).stylize(
      initStyle(fgBlue))

    echo tree.pstring()

  test "Base pprint":
    pprint 12
    pprint [1, 2, 3, 4]
    pprint ["hello"]
    pprint ("123", 0.3, nil)


suite "Deeply nested types":
  test "8D sequence":
    assertEq @[@[@[@[@[@[@[@[1]]]]]]]].pstr(),
         "[[[[[[[[1]]]]]]]]"

  test "4x4 seq":
    conf.maxWidth = 60
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(), "[[1, 2, 3, 4], [5, 6, 7, 8], [9, 1, 2, 3], [4, 5, 6, 7]]"


  test "Narrow 4x4 seq":
    conf.maxWidth = 20
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(), """
      - [1, 2, 3, 4]
      - [5, 6, 7, 8]
      - [9, 1, 2, 3]
      - [4, 5, 6, 7]""".dedent
    conf.maxWidth = 80


  test "Super narrow 2x2 seq":
    conf.maxWidth = 7
    assertEq @[
      @[1, 2, 4],
      @[5, 6, 8],
    ].pstr(), """
      - - 1
        - 2
        - 4
      - - 5
        - 6
        - 8""".dedent

    conf.maxWidth = 80

import json

suite "Printout json as object":
  test "Json named tuple":
    let jsonNode = parseJson("""{"key": 3.14}""")
    assertEq jsonNode.pstr(), "(key: 3.14)"

  test "Json array":
    assertEq parseJson("""{"key": [1, 2, 3]}""").pstr(),
        "(key: [1, 2, 3])"

  test "Json nested array":
    assertEq parseJson("""{"key": [[1, 2, 3], [1, 2, 3]]}""").pstr(),
        "(key: [[1, 2, 3], [1, 2, 3]])"


var jsonConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("{"), makeDelim("}")),
  fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
  fldSeparator: ",",
  kvSeparator: ": ",
  # wrapLargerThan: 10,
  alignFieldsRight: true
)

template pjson(arg: untyped): untyped =
  var conf = jsonConf
  conf.idCounter = makeCounter()
  toSimpleTree(arg, conf, @[]).prettyString(conf)

suite "Json pretty printing":
  test "Reparse int":
    let jsonNode = parseJson("""{"key": 3.14}""")
    let pretty = jsonNode.pjson()
    let reparsed = pretty.parseJson()
    assertEq jsonNode, reparsed

  test "Nested JSON":
    let jsonNode = parseJson """
       {
        "name":"John",
        "age":30,
        "cars": {
          "car1":"Ford",
          "car2":"BMW",
          "car3":"Fiat"
        }
       }""".dedent()

    let formatted = jsonNode.pjson()
    assertEq formatted, """
        {
          "name": "John",
           "age": 30,
          "cars": {"car1": "Ford", "car2": "BMW", "car3": "Fiat"}
        }""".dedent()

  test "Large JSON reparse":
    let jsonNode = parseJson """
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}        """

    assertEq jsonNode, jsonNode.pjson().parseJson()


var treeConf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  # wrapLargerThan: 10,
  nowrapMultiline: true
)

template treeStr(arg: untyped): untyped =
  var conf = treeConf
  conf.idCounter = makeCounter()
  toSimpleTree(arg, conf, @[]).prettyString(conf)

suite "Repr pprint":
  test "Lisp repr":
    assertEq pptConst("12").lispRepr(), "12"
    assertEq pptSeq(pptConst("12")).lispRepr(), "'(12)"
    assertEq pptObj("Hello", {"we": pptConst("12")}).lispRepr(),
      "(Hello :we 12)"
    assertEq pptObj("Hello", pptConst("12")).lispRepr(),
      "(Hello 12)"

  test "Tree repr":
    assertEq pptConst("12").treeRepr(), "12"
    assertEq pptSeq(pptConst("12"), pptConst("123123")).treeRepr(),
      "+-- 12\n+-- 123123"

    let obj = pptObj("Parent object",
                pptConst("12"),
                pptConst("12"),
                pptSeq(pptConst "Hello", pptConst "1231"),
                pptSeq("item", pptConst "Hello", pptConst "1231"),
                pptMap(("int", "float"), {
                  "Hello": pptConst("Workd"),
                  "Nice": pptSeq(pptConst("123"), pptConst("q234"))
      }),
      pptObj("Object name",
        pptConst("12"),
        pptConst("12"),
        pptConst("12")))

    echo obj.treeRepr(maxlevel = 1)
    echo obj.treeRepr()

  test "Colored tree repr":
    echo pptConst("hello", initPrintStyling(fg = fgRed)).treeRepr()
    echo pptSeq(
      pptConst("eowlr", initPrintStyling(fg = fgRed)),
      pptConst("eowlr", initPrintStyling(style = {styleUnderscore})),
    ).treeRepr()

    echo pptObj(
      "eeee",
      initPrintStyling(fg = fgYellow),
      {
        "fld1": pptConst("3333", initPrintStyling(fg = fgGreen)),
        "flde": pptConst("3333", initPrintStyling(fg = fgBlue))
      }
    ).treeRepr()

  test "Nested sequence":
    echo treeRepr(
      pptObj(
        "hello",
        {
          "fld1" : pptSeq(
            pptConst("123"),
            pptConst("xzzzz"),
            pptObj("test", {
              "sub" : pptConst("zzz"),
              "sub" : pptSeq(
                 pptConst("zzz"),
                 pptConst("zzz")
              )
            })
          ),
          "fld2" : pptConst("123")
        }
      )
    )

  test "Nested named object":
    echo treeRepr(
      pptObj("A", {
        "0" : pptObj("B", {
          "1" : pptSeq(pptConst("123"), pptConst("456")),
          "1" : pptSeq(pptConst("123"), pptConst("456")),
          "1" : pptSeq(pptConst("123"), pptConst("456")),
          "1" : pptSeq(pptConst("123"), pptConst("456"))
        })
      })
    )


suite "Large object printout":
  test "Large JSON as treeRepr":
    let jsonNode = parseJson """
      {"widget": {
          "debug": "on",
          "window": {
              "title": "Sample Konfabulator Widget",
              "name": "main_window",
              "width": 500,
              "height": 500
          },
          "image": {
              "src": "Images/Sun.png",
              "name": "sun1",
              "hOffset": 250,
              "vOffset": 250,
              "alignment": "center"
          },
          "text": {
              "data": "Click Here",
              "size": 36,
              "style": "bold",
              "name": "text1",
              "hOffset": 250,
              "vOffset": 100,
              "alignment": "center",
              "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
          }
      }}"""

    assertEq jsonNode.treeStr(), """
      widget:
        debug:  "on"
        window:
          title:  "Sample Konfabulator Widget"
          name:   "main_window"
          width:  500
          height: 500
        image:
          src:       "Images/Sun.png"
          name:      "sun1"
          hOffset:   250
          vOffset:   250
          alignment: "center"
        text:
          data:      "Click Here"
          size:      36
          style:     "bold"
          name:      "text1"
          hOffset:   250
          vOffset:   100
          alignment: "center"
          onMouseUp: "sun1.opacity = (sun1.opacity / 100) * 90;"""".dedent()


suite "Object tree to dot graph":
  # TODO generate html page with original object, it's pretty-printed
  # tree and resulting graph. It is not possible to adequately assert
  # generated. NOTE `unittest` has support for `setup` and `teardown`
  # sections: it can be used to generate report files.

  # TODO genrate images and html files only when
  # `-d:haxTestGenerateFiles` is enabled to avoid pollution of the
  # filesystem.
  template testgraph(obj: untyped): untyped =
    let graph = toDotGraph(obj)
    try:
      graph.topng("/tmp/image.png")
    except ShellExecError:
      let e = cast[ShellExecError](getCurrentException())
      let str = $graph
      echo "   ----   message:"
      echo e.msg
      echo "   ----   error str:"
      echo e.errstr
      echo "   ----   graph text:"
      echo str

  test "Integer":
    discard
    # testgraph(12)

  test "Integer sequence":
    discard
    # testgraph(@[12])


import ../src/hpprint/objdiff

suite "Object diff":
  test "ppDif simple":
    ppDiff 1, 2

  test "diff integers":
    assertEq diff(1, 2).paths(), @[@[0]]

  # NOTE test diff with string sequece too
  test "{diff} seq":
    assertEq diff(@[1], @[2]).paths(), @[@[0, 0]]
    assertEq diff(@[1, 1], @[2, 1]).paths(), @[@[0, 0]]
    assertEq diff(@[1, 2], @[2, 1]).paths(), @[@[0, 0], @[0, 1]]
    assertEq diff(@[1], @[1]).paths(), emptySeq[seq[int]]()
    assertEq diff(@["hel"], @["`1`"]).paths(), @[@[0, 0]]

  test "{diff} Object field difference":
    type
      U = object
        f1: int

    assertEq diff(U(f1: 90), U(f1: 91)).paths(), @[@[0, 0]]

  test "{diff} Case object difference":
    type
      U = object
        case kind: bool
          of true:
            f1: char
          of false:
            f2: string

    block:
      let res = diff(
        U(kind: true, f1: '1'),
        U(kind: true, f1: '9')
      )

      assertEq res.paths, @[@[0, 1]]

    block:
      let res = diff(
        U(kind: true, f1: '9'),
        U(kind: false, f2: "hello")
      )

      assertEq res.paths, @[@[0]]
      assertEq res[[0]].kind, odkKind
      # Not testig for different fields since they will not be
      # iterated (different kinds)

  type
    AstKind = enum
      akFloatLit
      akIntLit
      akStrLit

      akIdent

      akInfix
      akStmtList
      akTypeDef
      akCall

    Ast = object
      case kind: AstKind
        of akFloatLit:
          floatVal: float
        of akIntLit:
          intVal: int
        of akStrLit, akIdent:
          strVal: string
        of akStmtList, akTypeDef, akInfix, akCall:
          subnodes: seq[Ast]


  proc newTree(kind: AstKind, subn: varargs[Ast]): Ast =
    result = Ast(kind: kind)
    result.subnodes = toSeq(subn)

  proc newLit(a: float): Ast = Ast(kind: akFloatLit, floatVal: a)
  proc newLit(a: int): Ast = Ast(kind: akIntLit, intVal: a)
  proc newLit(a: string): Ast = Ast(kind: akStrLit, strVal: a)
  proc newIdent(a: string): Ast = Ast(kind: akIdent, strVal: a)
  proc newStmtList(args: varargs[Ast]): Ast =
    Ast(kind: akStmtList, subnodes: toSeq(args))

  test "{diff} Ast tree diff":
    let lhs = newStmtList(newLit("hello"), newLit(1.22))
    let rhs = newStmtList(newLit("hello"), newLit(1.23))
    ppDiff(lhs, rhs)

  test "{diff}":
    ppDiff({
      "A": newStmtList(newLit("hello"), newLit(1.22))
    }, {
      "A": newStmtList(newLit("hello"), newLit(1.23))
    })

  test "{diff} sequence item count":
    ppDiff(@[1, 2, 3], @[1, 2, 3, 4])
    ppDiff(@[1, 2, 3, 4], @[1, 2, 3])

  test "{annotate} set annotation for object":
    var tree = toObjTree(@[
      @[2, 3, 43, 4, 5],
      @[2, 3, 43, 4, 5],
    ])

    tree.mgetAtPath(@[0, 0]).annotate(" # Hello\n # World".toYellow())
    tree.mgetAtPath(@[0, 0, 1]).annotate(" # Hello".toRed())

    tree.pprint()
    tree.pprint(2)

suite "Other tests":
  test "primitive types colored":
    pprint "hello"
    pprint 12
    pprint '1'
    pprint @["12", "2"]

  test "Tuples and json colored":
    pprint %["he", "llo"]
    pprint %12

  test "Larger types colored":
    pprint PPrintConf()

  test "Cyclic objects":
    type
      A = ref object
        next: A

    var a = A()
    a.next = a

    pprint a

  test "Glob ignore fields":
    let val = (a: (b: (c: 10)))

    pprint val, ignore = @["a/b/c"]

  test "Enum array":
    type
      En = enum
        en1 = "sdf"
        en2
        en3

    block:
      var arr: array[En, string]
      pprint(arr)

    block:
      var arr: array[en1 .. en2, string]
      pprint(arr)
