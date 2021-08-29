# TODO account for control codes in stings

# TODO allow to discard certain elements after they have been
# generated. NOTE to not overcomplicate interface it should be done
# using `isAllowed(val: ObjTree)` instead of generic predicate.

# TODO allow to compress elements back if necessary (rst node
# represents each field as separate leaf)

# IDEA add support for forced single/multi line positioning of things
# like sequences.

# IDEA optionally annotate each genrated entry with type. This is
# useful for more complicated objects which contain different fields
# of different types and might benefid from annotaion type annotaion.
# Information is already stored in `ObjTree` so this is only a matter
# of crrectly identifying right/left fringes and writing information
# on them.

# TODO optionally ignore subfields and nodes. Can access paths for
# specifying patterns, and just compare elements using strings. And in
# general - configuration for what should be printed is quite important.
# Things like 100+-element sequences are fine if you generate HTML that can
# be collapsed, but not so much in terminal (adaptive configuration based
# on target backend? Optional coloring, respect `--color=never` and similar
# options?)

## Universal pretty-printer

import hnimast
export ObjPath
import hmisc/types/[hprimitives, colorstring]
import hmisc/algo/[hseq_distance, hseq_mapping, htemplates, hmath]
import hmisc/macros/introspection
import hmisc/core/all

when not defined(nimscript):
  import terminal

import std/[
  strformat, tables, strutils, sequtils, unicode, typetraits,
  macros, options, intsets, xmltree, strtabs, enumerate, sugar
]

export tables, options


#==========================  type declaration  ===========================#

type

  Delim* = object
    ## Block delimiters
    content*: string ## String for delimiter
    preferMultiline*: bool ## Don't try to put delimiter on the same
    ## line with content - always prefer new chunks

  DelimPair* = tuple[
    start: Delim,
    final: Delim
  ]

  StylingPosition = enum
    stpDefault
    stpTableKey
    stpTableVal

  PStyleConf* = object
    colored*: bool
    typeMapping*: proc(
      ctype: string, pos: StylingPosition): PrintStyling {.noSideEffect.}

  IdCounter = object
    now: int
    visitedAddrs: IntSet

  PPrintConf* = object
    ##[

Pretty print configuration

    ]##

    idCounter*: IdCounter
    sconf*: PStyleConf
    maxDepth*: int
    maxWidth*: int ## Max allowed width
    identStr*: string ## String to use for indentaion
    # wrapLargerThan*: int ##

    kvSeparator*: string ## String to use when separating key-value
    ## pairs.
    tblWrapper*: DelimPair ## Pair of delimiter around table instance

    objWrapper*: DelimPair ## Pair of delimiters around object instance
    fldNameWrapper*: DelimPair ## Pair of delimiter around table key
    fldSeparator*: string ## String to place between key-value pairs in list
    nowrapMultiline*: bool ## Do not wrap mutliline objects in delimiters
    alignFieldsRight*: bool ## Use right align on fields (default - left)

    seqSeparator*: string ## String to place between items in sequence
    seqPrefix*: string ## Prefix to use for multiline sequece
    ## instance. If empty multiline string will be wrapped in regular
    ## delimiters
    seqWrapper*: DelimPair ## Delimiters for sequence instance
    hideEmptyFields*: bool ## Hide empty fields (seq of zero length,
    ## `nil` references etc.).
    globIgnore*: seq[string]

    showPath*: bool ## Show glob-able path for objects


func next(cnt: var IdCounter): int =
  result = cnt.now
  inc cnt.now

func isVisited*[T](cnt: IdCounter, a: T): bool =
  when a is ref or a is ptr:
    cast[int](a) in cnt.visitedAddrs

  else:
    false

func visit*[T](cnt: var IdCounter, c: T) =
  when c is ref or c is ptr:
    cnt.visitedAddrs.incl cast[int](c)



const terminalPStyleConf* = PStyleConf(
  colored: true,
  typeMapping: (
    func(ctype: string, pos: StylingPosition): PrintStyling =
      result = initPrintStyling()
      if pos == stpTableKey:
        result.fg = fgGreen

      case ctype:
        of "string", "char", "seq[Rune]", "Rune":
          result.fg = fgYellow

        of "int", "float":
          result.fg = fgBlue

        of "nil", "bool":
          result.fg = fgCyan

        of "NimNode":
          result.style.incl styleItalic
  )
)


func isKVpairs(obj: ObjTree): bool =
  ## Check if entry should be printed as list of key-value pairs
  obj.kind == okTable or (obj.kind == okComposed and obj.namedFields)

import json

proc dedicatedConvertMatcher*[Obj](
  val: Obj, conv: proc(obj: Obj): ObjTree): ObjTree =
  ## Helper proc to correctly resolve overloading for pretty print
  ## converters
  return conv(val)

func getStyling*(
    conf: PStyleConf, ctype: string,
    stp: StylingPosition = stpDefault
  ): PrintStyling =
  if not conf.colored or isNil(conf.typeMapping):
    initPrintStyling()

  else:
    conf.typeMapping(ctype, stp)

func getStyling*(
    conf: PPrintConf, ctype: string, stp: StylingPosition = stpDefault
  ): PrintStyling =
  conf.sconf.getStyling(ctype, stp)

proc prettyPrintConverter*(
    val: JsonNode,
    conf: var PPrintConf,
    path: ObjPath,
  ): ObjTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  let nilval = ObjTree(
    constType: "nil", kind: okConstant,
    strLit: "null", styling: conf.sconf.getStyling("nil"))
  if isNil(val):
    nilval

  else:
    case val.kind:
      of JNull:
        nilval

      of JBool:
        ObjTree(
          constType: "bool", kind: okConstant,
          strLit: $val.getBool(), styling: conf.sconf.getStyling("bool"))
      of JInt:
        ObjTree(
          constType: "int", kind: okConstant,
          strLit: $val.getInt(), styling: conf.sconf.getStyling("int"))
      of JFloat:
        ObjTree(
          constType: "float", kind: okConstant,
          strLit: $val.getFloat(), styling: conf.sconf.getStyling("float"))
      of JString:
        ObjTree(
          constType: "string", kind: okConstant,
          strLit: &"\"{val.getStr()}\"",
          styling: conf.sconf.getStyling("string")
        )
      of JArray:
        ObjTree(
          kind: okSequence, styling: conf.sconf.getStyling("seq"),
          valItems: val.getElems().mapPairs(
            prettyPrintConverter(rhs, conf, path & seqAccs(idx))
          )
        )
      of JObject:
        ObjTree(
          kind: okComposed,
          namedFields: true,
          namedObject: false,
          styling: conf.sconf.getStyling("object"),
          fldPairs: val.getFields().mapPairs((
            name: lhs,
            value: prettyPrintConverter(
              rhs, conf, path & seqAccs(idx))
          )))


proc prettyPrintConverter*(
    val: XmlNode,
    conf: var PPrintConf,
    path: ObjPath,
  ): ObjTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  let nilval = ObjTree(
    constType: "nil", kind: okConstant,
    strLit: "null", styling: conf.sconf.getStyling("nil"))
  if isNil(val):
    result = nilval

  else:
    case val.kind:
      of xnVerbatimText, xnEntity, xnText, xnCData:
        let name =
          case val.kind:
            of xnVerbatimText: "verbatim"
            of xnCData: "<CDATA"
            of xnText: "text"
            of xnEntity: "entity"
            else: ""

        result = ObjTree(
          constType: name, kind: okConstant,
          strLit: &"\"{val.innerText()}\"",
          styling: conf.sconf.getStyling("string")
        )

      of xnComment:
        let text: string = xmltree.text(val)
        result = ObjTree(
          constType: "string", kind: okConstant,
          strLit: &"<!-- {text}",
          styling: conf.sconf.getStyling("xnComment")
        )

      of xnElement:
        result = ObjTree(
          kind: okComposed,
          namedFields: true,
          namedObject: true,
          name: "<" & val.tag() & ">",
          styling: conf.sconf.getStyling("XML"),
        )

        if not isNil(attrs(val)):
          for key, val in pairs(attrs(val)):
            result.fldPairs.add((
              key,
              ObjTree(kind: okConstant, strLit: val,
                      styling: conf.sconf.getStyling("string"))
            ))

        var subnodes: seq[ObjTree]
        for node in items(val):
          subnodes.add prettyPrintConverter(
            node, conf, path & objAccs(val.tag))

        result.fldPairs.add(("", ObjTree(
          kind: okSequence,
          valItems: subnodes,
          styling: conf.sconf.getStyling("")
        )))

proc prettyPrintConverter*(
  val: seq[Rune], conf: var PPrintConf, path: ObjPath): ObjTree =
  ObjTree(
    styling: conf.sconf.getStyling($typeof(val)),
    kind: okConstant,
    constType: "seq[Rune]",
    strLit: &"\"{val}\""
  )


proc toSimpleTree*[Obj](
    entry: Obj,
    conf: var PPrintConf,
    path: ObjPath
  ): ObjTree

proc prettyPrintConverter*[R1, R2](
  val: HSlice[R1, R2], conf: var PPrintConf, path: ObjPath): ObjTree =
  ObjTree(
    styling: conf.sconf.getStyling($typeof(val)),
    kind: okSequence,
    itemType: $typeof(typeof(R1)),
    objId: conf.idCounter.next(),
    valItems: @[
      toSimpleTree(val.a, conf, path),
      toSimpleTree(val.b, conf, path)
    ]
  )


proc prettyPrintConverter*(
  val: NimNode, conf: var PPrintConf, path: ObjPath): ObjTree =
  # TODO can add syntax hightlight to string literal generated from
  #      nim node
  ObjTree(
    styling: conf.sconf.getStyling("NimNode"),
    kind: okConstant,
    constType: "NimNode",
    strLit: val.toStrLit().strVal()
  )

template unref(val: untyped): untyped =
  when val is ref: val[]
  else: val

func checkPrimitive(tree: ObjTree): bool =
  ##[

Check if tree can be considered primitive.

NOTE: values are not checked for primitivenes recursively. Instead
`isPrimitive` field is used.

  ]##
  case tree.kind:
    of okConstant:
      tree.constType in @["string", "int", "float"]

    of okSequence:
      (tree.valItems.len < 5) and
      tree.valItems.allOfIt(it.isPrimitive)

    of okTable:
      (tree.valPairs.len < 5) and
      tree.valPairs.allOfIt(it.val.isPrimitive)

    of okComposed:
      (tree.fldPairs.len < 5) and
      tree.fldPairs.allOfIt(it.value.isPrimitive)

func toJsonPtr*(path: ObjPath): string =
  for idx, element in pairs(path):
    if idx > 0:
      result &= "/"

    case element.kind:
      of okConstant: result &= ""
      of okSequence: result &= $element.idx
      of okTable: result &= element.key
      of okComposed: result &= element.name

proc ignoredBy*(conf: PPrintConf, path: ObjPath): Option[string] =
  if conf.globIgnore.len > 0:
    for glob in conf.globIgnore:
      if gitignoreGlobMatch(path.toJsonPtr(), glob):
        return some(glob)

proc toSimpleTree*[Obj](
    entry: Obj,
    conf: var PPrintConf,
    path: ObjPath
  ): ObjTree =
  ## Top-level dispatch for pretty-printing
  ##
  ## Generic implementation for pretty-print conveter for types not
  ## implementing dedicated `prettyPrintConverter`
  mixin prettyPrintConverter
  mixin items
  mixin pairs

  defer:
    result.isPrimitive = result.checkPrimitive()

  let ignoredBy = conf.ignoredBy(path)
  if conf.idCounter.isVisited(entry) or ignoredBy.isSome():
    result = ObjTree(
      styling: conf.sconf.getStyling($typeof(Obj)),
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: "",
      path: path,
      objId: conf.idCounter.next()
    )

    if conf.idCounter.isVisited(entry):
      result.strLit = "<visited>"
      when entry is ref or entry is ptr:
        result.strLit &= " at " & $toRed($cast[int](entry))

    else:
      result.strLit = "<ignored>"
      result.ignored = true
      # result.ignoredBy = ignoredBy

    return

  if conf.maxDepth > 0 and path.len > conf.maxDepth:
    result = ObjTree(
      styling: conf.sconf.getStyling($typeof(Obj)),
      kind: okConstant,
      constType: $typeof(Obj),
      path: path,
      objId: conf.idCounter.next(),
      strLit: "<max depth reached>",
      ignored: true
    )

    return



  conf.idCounter.visit(entry)

  when compiles(prettyPrintConverter(entry, conf, path)):
    # If dedicated implementation exists, use it
    result = prettyPrintConverter(entry, conf, path)
    return

  elif entry is typeof(nil):
    return ObjTree(
      styling: conf.sconf.getStyling("nil"),
      kind: okConstant,
      strLit: "nil",
      path: path,
    )

  elif not ( # key-value pairs (tables etc.)
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):

    # IMPLEMENT Insert values in sorted order to give the same layout
    # for unordered containers
    var keyType: string
    for key, val in pairs(entry):
      keyType = $typeof(key)

    result = ObjTree(styling: conf.sconf.getStyling($typeof(entry)),
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2)),
      path: path,
      objId: conf.idCounter.next(),
      keyStyling: conf.sconf.getStyling(keyType, stpTableKey)
    )

    for key, val in pairs(entry):
      when key is ref:
        if isNil(key):
          result.valPairs.add(("<nil-key>", "?"))
          continue

      when val is ref:
        if isNil(val):
          # result.valPairs.add(($key, "<nil>"))
          continue

      let res = toSimpleTree(val, conf, path & tableAccs($key))

      if not res.ignored:
        result.valPairs.add(($key, res))

    result.valPairs.sort do (x, y: auto) -> int:
      cmp(x[0], y[0])

  elif (entry is array) and
       (
         when compiles(genericParams(typeof entry)):
           get(genericParams(typeof entry), 0) is (
             StaticParam[char] or static[char] or char or
             StaticParam[enum] or static[enum] or enum
           )
         else:
           false
       )
    :
    type ArrKey = get(genericParams(typeof entry), 0)
    type ArrValue = get(genericParams(typeof entry), 1)
    mixin items
    result = ObjTree(
      styling: conf.sconf.getStyling($typeof(entry)),
      kind: okTable,
      keyType: $typeof(ArrKey),
      valType: $typeof(ArrValue),
      path: path,
      objId: (entry is ref).tern(
        cast[int](unsafeAddr entry),
        conf.idCounter.next()
      ),
      keyStyling: conf.sconf.getStyling($typeof(ArrKey), stpTableKey)
    )

    for key, val in pairs(entry):
      when ArrKey is (StaticParam[enum] or static[enum] or enum):
        when key is range:
          # FIXME get underlying enum type instead of `range[]`.
          # `directEnumName` uses `getTypeImpl`, which cannot handle ranges
          # correctly. It can be fixed here, or in
          # `hmisc/macros/introspection`, but I would prefer to figure out
          # the way to implement it with `std/typetraits` if possible.

          # The question is: how to get type of `array` range using
          # `typetraits` (or somehow else)? - I can get to `range
          # 0..3(int)` using `genericParams` and `get()`, but I cannot use
          # them repeatedly - `echo genericParams(array[0 .. 3,
          # int]).get(0).genericParams()` fails to compile with
          #
          # ```nim
          # Error: type expected, but got:
          # typeof(default:
          #   type
          #     T2`gensym5 = array[0 .. 3, int]
          #   (range[0 .. 3], int)[0])
          # ```

          let keyName = directEnumName(key)

        else:
          let keyName = directEnumName(key)

      else:
        let keyName = $key

      let res = toSimpleTree(val, conf, path & tableAccs(keyName))

      if not res.ignored:
        result.valPairs.add((keyName, res))

  elif not ( # sequences but not strings
      (entry is string) or
      (entry is char)
    ) and (
    (
      (compiles(for i in items(entry): discard)) or
      (compiles(for i in items(entry[]): discard))
    ) and (not compiles(entry.kind))
    # Iterable objects with `.kind` field are most likely to be some form
    # of AST
  ):
    mixin items
    const directItems = compiles(for i in items(entry): discard)

    result = ObjTree(styling: conf.sconf.getStyling($typeof(entry)),
      kind: okSequence,
      itemType: $typeof(
        when directItems:
          items(entry)

        else:
          items(entry[])
      ),
      objId: (entry is ref).tern(
        cast[int](unsafeAddr entry),
        conf.idCounter.next()
      )
    )

    var idx: int = 0
    for it in (
      when directItems:
        items(entry)

      else:
        items(entry[])
    ):
      let res = toSimpleTree(it, conf, path & seqAccs(idx))

      if not res.ignored:
        result.valItems.add(res)

      inc idx

  elif not (entry is Option) and
       (
         (entry is object) or # objects
         (entry is ref object) or
         (entry is tuple)):
    let id: int =
      when entry is ref:
        when nimvm:
          when compiles(hash(entry)):
            hash(entry)
          else:
            0
        else:
          cast[int](unsafeAddr entry)
      else:
        conf.idCounter.next()

    when (entry is object) or (entry is ref object):
      result = ObjTree(
        styling: conf.sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: true,
        namedFields: true,
        objId: id,
        path: path
      )

    elif isNamedTuple(Obj):
      result = ObjTree(styling: conf.sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: false,
        namedFields: true,
        objId: id,
        path: path
      )

    else:
      result = ObjTree(styling: conf.sconf.getStyling($typeof(entry)),
        kind: okComposed,
        namedFields: false,
        namedObject: false,
        objId: id
      )

    result.path = path

    when (entry is ref object):
      if isNil(entry):
        result = ObjTree(
          styling: conf.sconf.getStyling($typeof(entry)),
          kind: okConstant,
          constType: $(typeof(Obj)),
          strLit: "nil",
          objId: conf.idCounter.next(),
          path: path
        )

      else:
        var idx: int = 0
        for name, value in fieldPairs(entry[]):
          let res = toSimpleTree(value, conf, path & objAccs(name))
          if not res.ignored:
            result.fldPairs.add((name, res))

          inc idx

    else:
      var idx: int = 0
      for name, value in fieldPairs(entry):
        var ok = true
        when value is ref:
          if isNil(value):
            ok = false


        if ok:
          let res = toSimpleTree(value, conf, path & objAccs(name))

          if not res.ignored:
            result.fldPairs.add((name, res))

        inc idx


  elif (entry is proc): # proc type
    result = ObjTree(
      styling: conf.sconf.getStyling($typeof(entry)),
      kind: okConstant,
      constType: $(typeof(Obj)),
      strLit: $(typeof(Obj)),
      path: path,
      objId: conf.idCounter.next()
    )

  elif entry is Option:
    if entry.isSome():
      result = ObjTree(
        styling: conf.sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: false,
        namedFields: false,
        objId: 1, # HACK
        fldPairs: @[(
          name: "val",
          value: toSimpleTree(entry.get(), conf, path))])

    else:
      result = ObjTree(
        styling: conf.sconf.getStyling($typeof(entry)),
        kind: okConstant,
        constType: $typeof(Obj),
        strLit: "none",
        path: path,
        objId: conf.idCounter.next()
      )

  else: # everything else
    # echov typeof entry
    var style = conf.sconf.getStyling($typeof(entry))
    when entry is string:
      let val = "\"" & entry & "\""

    elif entry is pointer:
      let val = "<pointer>"

    elif entry is void:
      let val = "<void>"

    elif entry is Rune:
      let val = "\'" & $(@[entry]) & "\'"

    elif entry is NimNode:
      let val = entry.treeRepr()

    elif entry is (SomeNumber | bool):
      let val = $entry

    elif entry is enum:
      let val = $entry

    else:
      when entry is distinct and not compiles($entry):
        let val = $distinctBase(entry)

      else:
        let val = $entry

    result = ObjTree(styling: style,
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: val,
      path: path,
      objId: conf.idCounter.next()
    )


  if conf.showPath:
    let tmp = ObjTree(
      styling: initPrintStyling(),
      kind: okComposed,
      fldPairs: @[(name: toJsonPtr(path), value: result)]
    )

    return tmp


type
  Chunk* = object
    content: seq[string] ## Lines for chunk
    maxWidth: int ## Max line lenght in chunk
    styling: PrintStyling

  KVPair* = object
    name: string
    val: Chunk
    annotation: string

func kvPair*(name: string, val: Chunk, annotation: string): KVPair =
  KVPair(name: name, val: val, annotation: annotation)

func `[]`(ch: Chunk, idx: int): string =
  if idx >= ch.content.len:
    ""
  else:
    ch.content[idx]

type
  RelPos* = enum
    ## Relative position of label to chunk
    rpBottomRight
    # [|||||||]
    # [|||||||] <label>
    rpTopLeftAbove
    # <label>
    #   [|||||||]
    #   [|||||||]
    rpTopLeftLeft
    # <label> [|||||||]
    #         [|||||||]
    rpBottomLeft
    #   [|||||||]
    #   [|||||||]
    # <label>
    rpPrefix
    # <label>[|||||||]
    # <label>[|||||||]

proc lineCount(c: Chunk): int = c.content.len()
func toString*(c: Chunk, indent: int = 0): string =
  c.content.mapIt(" ".repeat(indent) & it).
    join("\n").styleTerm(c.styling)


proc `$`*(c: Chunk): string = c.toString()

func multiline(chunk: Chunk): bool = chunk.content.len > 1
func multiline(str: string): bool = str.anyOfIt(it == '\n')

func empty(conf: Delim): bool = conf.content.len == 0
func makeDelim*(str: string, multiline: bool = false): Delim =
  Delim(
    # appendNew: str.startsWith('\n'),
    # prependNew: str.endsWith('\n'),
    content: str.strip(chars = {'\n'}),
    preferMultiline: multiline
  )

proc makeChunk*(content: seq[string]): Chunk =
  ## Create chunk from list of lines
  Chunk(
    content: content,
    maxWidth: content.mapIt(it.termLen()).max(0)
  )

proc makeChunk*(content: string): Chunk =
  ## Create chunk from string
  Chunk(content: @[content], maxWidth: content.termLen())

proc makeChunk*(other: seq[Chunk]): Chunk =
  ## Create chunk from lines in other chunks
  result = Chunk(
    content: sequtils.concat(other.mapIt(it.content))
  )

  result.maxWidth = result.content.mapIt(it.termLen()).max(0)

type
  ChunkLabels* = Table[RelPos, tuple[text: string, offset: int]]


proc relativePosition*(
  chunk: Chunk,
  labelsIn:
    ChunkLabels |
    seq[(RelPos, tuple[text: string, offset: int])],
  ignored: set[RelPos] = {}): Chunk =
  ## Position mutliple labels arounc chunk. Labels on `ignored`
  ## positions will be ingored

  let labels: ChunkLabels =
    when labelsIn is Table:
      labelsIn
    else:
      labelsIn.toTable()

  var
    leftPad = 0
    rightPad = 0
    topPad = 0
    bottomPad = 0

  let prefixOverride = (rpPrefix in labels)

  if prefixOverride and (rpTopLeftLeft in labels or rpBottomRight in labels):
    raise newArgumentError(
      "Incompatible chunk labels - prefix&top-left-left or prefix&bottom-right")

  for pos, label in labels:
    case pos:
      of rpBottomRight:
        rightPad = label.text.termLen

      of rpTopLeftAbove:
        topPad = 1
        leftPad = max(label.offset, leftPad)

      of rpTopLeftLeft:
        leftPad = max(leftPad, label.text.termLen)

      of rpBottomLeft:
        bottomPad = 1
        leftPad = max(leftPad, label.offset)

      of rpPrefix:
        leftPad = label.text.termLen

  let resWidth = chunk.maxWidth + leftPad + rightPad
  let resHeight = chunk.lineCount + topPad + bottomPad

  var resLines: seq[string]
  if rpTopLeftAbove in labels:
    resLines.add labels[rpTopLeftAbove].text

  let pref =
    if prefixOverride:
      labels[rpPrefix].text
    else:
      " ".repeat(leftPad)

  for idx, line in chunk.content:
    if idx == 0 and (rpTopLeftLeft in labels):
      resLines.add(labels[rpTopLeftLeft].text &
        line.styleTerm(chunk.styling))
    else:
      resLines.add(pref & line.styleTerm(chunk.styling))

  if (rpBottomRight in labels) and
     (rpBottomRight notin ignored) and
     (resLines.len > 0):
    resLines[^1] &= labels[rpBottomRight].text

  if rpBottomLeft in labels:
    resLines.add labels[rpBottomLeft].text

  return makeChunk(resLines.mapIt(strutils.strip(it, leading = false)))


proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk

proc getWrapperConf(
  current: ObjTree, conf: PPrintConf): tuple[start, final: Delim] =
  let (wrapBeg, wrapEnd) = # Delimiters at the start/end of the block
    case current.kind:
      of okComposed:
        if current.namedObject:
          var objWrap = conf.objWrapper
          objWrap.start.content = &"{current.name}{objWrap.start.content}"
          objWrap
        else:
          conf.objWrapper
      of okSequence:
        conf.seqWrapper
      of okTable:
        # TODO use configurable wrapper begin/end
        conf.tblWrapper
      else:
        assert false, "Cannot arrange kv pair in constant"
        (makeDelim("."), makeDelim("."))

  return (wrapBeg, wrapEnd)


proc getLabelConfiguration(
  conf: PPrintConf, current: ObjTree, ident: int): tuple[
  item, blocks: ChunkLabels,
  widthconf: (int, int)] =
  ## Get label configuration

  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)
  var subIdent = 0 # Required right margin for field values
  var maxWidth = 0 # Max allowed withd
  var itemLabels: ChunkLabels
  var blockLabels: ChunkLabels
  let offset = conf.identStr.termLen # Internal offset between prefix
                                     # delimiter and chunk body
  let prefixOverride = (current.kind == okSequence) and
    (conf.seqPrefix.termLen > 0)

  # match((prefixOverride, wrapBeg.preferMultiline, wrapEnd.preferMultiline)):
  if prefixOverride:
  # (true, _, _):
    subIdent = conf.seqPrefix.termLen
    maxWidth = conf.maxWidth
    itemLabels[rpTopLeftLeft] = (text: conf.seqPrefix, offset: 0)
  elif wrapBeg.preferMultiline and wrapEnd.preferMultiline:
  # (_, true, true):
    # (prefix delimiter)
    # <field> [ block block
    #           block block ]
    # (suffix delimiter)
    subIdent = ident
    maxWidth = conf.maxWidth
    itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
    itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: offset)

  elif wrapBeg.preferMultiline and (not wrapEnd.preferMultiline):
  # (_, true, false):
    # (prefix delimiter)
    # <field> [ block block
    #           block block ] (suffix delimiter)
    subIdent = ident
    maxWidth = conf.maxWidth - wrapEnd.content.len()
    itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
    itemLabels[rpBottomRight] = (text: wrapEnd.content, offset: 0)
  elif (not wrapBeg.preferMultiline) and wrapEnd.preferMultiline:
  # (_, false, true):
    # (prefix delimiter) <field> [ block block
    #                              block block ]
    # (suffix delimiter)
    subIdent = ident + wrapBeg.content.len()
    # IMPLEMENT account for sequence prefix, kvSeparator etc.
    maxWidth = conf.maxWidth
    itemLabels[rpTopLeftLeft] = (text: wrapBeg.content, offset: 0)
    itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 0)
  elif (not wrapBeg.preferMultiline) and (not wrapEnd.preferMultiline):
  # (_, false, false):
    # (prefix delimiter) <field> [ block block
    #                              block block ] (suffix delimiter)
    subIdent = ident + wrapBeg.content.len()
    maxWidth = conf.maxWidth - wrapEnd.content.len()
    case current.kind:
      of okSequence:
        itemLabels[rpBottomRight] = (text: conf.seqSeparator, offset: 0)
        blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
        blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
      of {okTable, okComposed}:
        itemLabels[rpBottomRight] = (text: conf.fldSeparator, offset: 0)
        # TODO more fine-grained configuration for different
        # wrapping settings.
        if not conf.nowrapMultiline:
          blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
          blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
        elif current.kind == okComposed and current.namedObject:
          blockLabels[rpTopLeftAbove] = (text: current.name, offset: 2)
      else:
        discard

  return (
    item: itemLabels,
    blocks: blockLabels,
    widthconf: (subIdent, offset)
  )



proc arrangeKVPairs(
  input: seq[KVPair],
  conf: PPrintConf, current: ObjTree, ident: int): Chunk =
  ## Layout sequence of key-value pairs. `name` field in tuple items
  ## might be empty if `current.kind` is `okSequence`.
  ##
  ## - TODO :: Align elements from 2d sequences and lists of one-line
  ##   object's fields
  # if current.annotation.len > 0:
  #   echo current.annotation, " ", current.kind
  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)

  let trySingleLine = (not input.anyOfIt(it.val.multiline())) and
    (not input.anyOfIt(it.annotation.multiline()))

  if trySingleLine:
    var singleLine =
      if current.kind == okSequence or (
        current.kind == okComposed and (not current.namedFields)):
        input.mapIt(tern(
          it.val.content.len > 0,
          &"{it.val.content[0]}",
          ""
        )).join(conf.seqSeparator)

      else:
        input.mapIt(&"{it.name}{conf.kvSeparator}{it.val[0]}").join(
          conf.seqSeparator)

    singleLine = wrapBeg.content & singleLine & wrapEnd.content
    let
      annotLines = current.annotation.splitColor("\n")
      maxAnnWidth = annotLines.maxIt(it.termLen())
      singleLen = singleLine.termLen

    if singleLen + maxAnnWidth < (conf.maxWidth - ident):
      return makeChunk(content = @[
        singleLine & annotLines[0]
      ] & annotLines[1..^1].mapIt(" ".repeat(singleLen) & it))

  # Try positioning on multiple lines
  let fldsWidth =
    # Width of the larges field
    if current.isKVPairs():
      input.mapIt(it.name.termLen()).max(0) + conf.kvSeparator.len()
    else:
      0

  proc makeFldName(it: KVPair): (RelPos, (string, int)) =
    let pos = case current.kind:
      of okSequence: rpTopLeftLeft
      of okTable, okComposed: (it.val.multiline()).tern(rpTopLeftAbove, rpTopLeftLeft)
      of okConstant: rpPrefix

    let text = case current.kind:
      of okComposed, okTable:
        if conf.alignFieldsRight:
          strutils.align(it.name & conf.kvSeparator, fldsWidth)
        else:
          strutils.alignLeft(it.name & conf.kvSeparator, fldsWidth)
      of okSequence:
        ""
      of okConstant:
        raiseAssert("Invalid current kind: constant")

    return (pos, (text # & it.annotation
                         , conf.identStr.termLen()))

  let (itemLabels, blockLabels, widthConf) =
    getLabelConfiguration(conf = conf, current = current, ident = ident)

  result = ((
    block:
      collect(newSeq):
        for idx, it in pairs(input):
          relativePosition(it.val, (@[makeFldName(it)])).
            relativePosition(
              itemLabels,
              ignored = (idx == input.len() - 1).tern({rpBottomRight}, {}))))
  .makeChunk()
  .relativePosition(blockLabels)

proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk =
  # debugecho conf.maxWidth

  case current.kind:
    of okConstant:

      var clines: seq[string] = current.strLit.styleTerm(
        current.styling).splitSGR_sep().mapIt(it.toString())

      if clines.len > 0:
        clines[^1] &= current.annotation
        result = makeChunk(content = clines)
      else:
        result = makeChunk(content = @[""])
    of okComposed:
      let maxFld = current.fldPairs.mapIt(
        it.name.termLen() + conf.fldNameWrapper.start.content.len() +
        conf.fldNameWrapper.start.content.len()
      ).max(0)

      var tmp: seq[KVPair]
      for it in current.fldPairs:
        # echo it.value
        tmp.add kvPair(
          conf.fldNameWrapper.start.content & it.name &
            conf.fldNameWrapper.final.content,
          pstringRecursive(it.value, conf, maxFld + ident),
          it.value.annotation
        )

      result = tmp.arrangeKVPairs(conf, current, ident + maxFld)
    of okTable:
      # FIXME print key/value/table (in case this is a non `Table`
      # mapping) types
      let maxFld = current.valPairs.mapIt(it.key.termLen()).max(0)
      # FIXME align key-value pairs
      result = current.valPairs.mapIt(
        kvPair(
          it.key.styleTerm(current.keyStyling),
          pstringRecursive(it.val, conf, maxFld + ident),
          it.val.annotation
        )
      ).arrangeKVPairs(conf, current, ident + maxFld)
    of okSequence:
      var tmp: seq[KVPair]
      for it in current.valItems:
        # IDEA optionally use element index as key
        tmp.add kvPair(
          "",
          pstringRecursive(it, conf, ident + conf.seqPrefix.termLen()),
          it.annotation
        )

        # echo it.annotation

      result = tmp.arrangeKVPairs(conf, current, ident)
  result.styling = current.styling
  # echo result.toString()
  # echo result.content


proc prettyString*(tree: ObjTree,
                   conf: var PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string using configuration `conf`
  var newConf = conf
  newConf.maxWidth = conf.maxWidth - ident
  let chunks = pstringRecursive(tree, newConf, ident = ident)
  result = chunks.toString(ident)


let objectPPrintConf* = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  sconf: terminalPStyleConf
  # hideEmptyFields: true # FIXME does not seem to work
)


func getSubnodes*(it: ObjTree): seq[ObjTree] =
  case it.kind:
    of okConstant: raise newArgumentError("No sub for `okConstant`")
    of okSequence: it.valItems
    of okTable: it.valPairs.mapIt(it.val)
    of okComposed: it.fldPairs.mapIt(it.value)


func makeCounter*(): IdCounter = IdCounter()
proc pstring*[Obj](
    obj: Obj, ident: int = 0,
    maxWidth: int = 80,
    ignore: seq[string] = @[],
    showPath: bool = false,
    maxDepth: int = 120,
    sconf: PStyleConf = terminalPStyleConf
  ): string =
  var conf = PPrintConf(
    maxWidth: 80,
    identStr: "  ",
    seqSeparator: ", ",
    seqPrefix: "- ",
    seqWrapper: (makeDelim("["), makeDelim("]")),
    objWrapper: (makeDelim("("), makeDelim(")")),
    tblWrapper: (makeDelim("{"), makeDelim("}")),
    kvSeparator: ": ",
    sconf: terminalPStyleConf,
    nowrapMultiline: true
    # hideEmptyFields: true # FIXME does not seem to work
  )

  conf.idCounter = makeCounter()
  conf.maxWidth = maxWidth
  conf.globIgnore = ignore
  conf.showPath = showPath
  conf.maxDepth = maxDepth
  prettyString(toSimpleTree(obj, conf, @[]), conf, ident)


proc pstring*[Obj](obj: Obj, conf: PPrintConf): string =
  var counter = makeCounter()
  prettyString(toSimpleTree(obj, counter), conf, 0)

proc pstring*(obj: ObjTree, ident: int = 0, maxWidth: int = 80): string =
  var conf = objectPPrintConf
  conf.maxWidth = maxWidth
  prettyString(obj, conf, ident)

proc pprint*(tree: ObjTree, maxw: int = 80): void =
  # FIXME `maxw` does not work correctly
  var conf = objectPPrintConf
  conf.maxWidth = maxw
  echo prettyString(tree, conf)


proc toObjTree*[Obj](obj: Obj): ObjTree =
  var conf = objectPPrintConf
  conf.idCounter = makeCounter()
  toSimpleTree(obj, conf, @[])

proc pprintAtPath*[Obj](obj: Obj, path: TreePath): void =
  var counter = makeCounter()
  pprint toSimpleTree(obj, counter).getAtPath(path)

proc pprint*[Obj](
    obj: Obj, ident: int = 0,
    maxWidth: int = 110,
    ignore: seq[string] = @[],
    showPath: bool = false,
    maxDepth: int = 120
  ): void =
  echo pstring(obj, ident, maxWidth,
               ignore = ignore, showPath = showPath,
               maxDepth = maxDepth)
  # var conf = objectPPrintConf
  # conf.maxWidth = maxWidth
  # echo prettyString(toSimpleTree(obj), conf, ident)

func debugPPrint*[Obj](obj: Obj): void =
  {.noSideEffect.}:
    pprint(obj)
