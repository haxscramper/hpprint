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
import hmisc/types/[hprimitives, colorstring]
import hmisc/helpers

when not defined(nimscript):
  import terminal

import strformat, tables, strutils, sequtils, unicode, typetraits, macros,
       options


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

  PPrintConf* = object
    ##[

Pretty print configuration

    ]##

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

        of "nil":
          result.fg = fgCyan
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

func getStyling(
    conf: PStyleConf, ctype: string,
    stp: StylingPosition = stpDefault
  ): PrintStyling =
  if not conf.colored or isNil(conf.typeMapping):
    initPrintStyling()

  else:
    conf.typeMapping(ctype, stp)

proc prettyPrintConverter(
    val: JsonNode,
    sconf: PStyleConf,
    path: ObjPath,
  ): ObjTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  let nilval = ObjTree(
    constType: "nil", kind: okConstant,
    strLit: "null", styling: sconf.getStyling("nil"))
  if isNil(val):
    nilval

  else:
    case val.kind:
      of JNull:
        nilval

      of JBool:
        ObjTree(
          constType: "bool", kind: okConstant,
          strLit: $val.getBool(), styling: sconf.getStyling("bool"))
      of JInt:
        ObjTree(
          constType: "int", kind: okConstant,
          strLit: $val.getInt(), styling: sconf.getStyling("int"))
      of JFloat:
        ObjTree(
          constType: "float", kind: okConstant,
          strLit: $val.getFloat(), styling: sconf.getStyling("float"))
      of JString:
        ObjTree(
          constType: "string", kind: okConstant,
          strLit: &"\"{val.getStr()}\"",
          styling: sconf.getStyling("string")
        )
      of JArray:
        ObjTree(
          kind: okSequence, styling: sconf.getStyling("seq"),
          valItems: val.getElems().mapPairs(
            prettyPrintConverter(rhs, sconf, path = path & seqAccs(idx))
          )
        )
      of JObject:
        ObjTree(
          kind: okComposed,
          namedFields: true,
          namedObject: false,
          styling: sconf.getStyling("object"),
          fldPairs: val.getFields().mapPairs((
            name: lhs,
            value: prettyPrintConverter(
              rhs, sconf, path = path & seqAccs(idx))
          )))

proc prettyPrintConverter(
  val: seq[Rune], sconf: PStyleConf, path: seq[int] = @[0]): ObjTree =
  ObjTree(
    styling: sconf.getStyling($typeof(val)),
    kind: okConstant,
    constType: "seq[Rune]",
    strLit: &"\"{val}\""
  )

proc prettyPrintConverter(val: NimNode, path: seq[int] = @[0]): ObjTree =
  # TODO can add syntax hightlight to string literal generated from
  #      nim node
  ObjTree(
    styling: initPrintStyling(),
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

type
  IdCounter = object
    now: int

func next(cnt: var IdCounter): int =
  result = cnt.now
  inc cnt.now

proc toSimpleTree*[Obj](
    entry: Obj,
    idCounter: var IdCounter,
    sconf: PStyleConf,
    conf: PPrintConf,
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

  when compiles(prettyPrintConverter(entry, sconf, path = path)):
    # If dedicated implementation exists, use it
    return prettyPrintConverter(entry, sconf, path = path)

  elif entry is typeof(nil):
    return ObjTree(
      styling: sconf.getStyling("nil"),
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

    result = ObjTree(styling: sconf.getStyling($typeof(entry)),
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2)),
      path: path,
      objId: idCounter.next(),
      keyStyling: sconf.getStyling(keyType, stpTableKey)
    )

    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val,
          idCounter, sconf = sconf, conf = conf,
          path = path & tableAccs($key)
      )))

    result.valPairs.sort do (x, y: auto) -> int:
      cmp(x[0], y[0])

  elif not ( # sequences but not strings
      (entry is string) or
      (entry is char)
    ) and (
    (compiles(for i in items(entry): discard)) or
    (compiles(for i in items(entry[]): discard))
  ):
    mixin items
    const directItems = compiles(for i in items(entry): discard)

    result = ObjTree(styling: sconf.getStyling($typeof(entry)),
      kind: okSequence,
      itemType: $typeof(
        when directItems:
          items(entry)

        else:
          items(entry[])
      ),
      objId: (entry is ref).tern(
        cast[int](unsafeAddr entry),
        idCounter.next()
      )
    )

    var idx: int = 0
    for it in (
      when directItems:
        items(entry)

      else:
        items(entry[])
    ):
      result.valItems.add(toSimpleTree(
        it, path = path & seqAccs(idx),
        sconf = sconf,
        idCounter = idCounter,
        conf = conf
      ))
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
        idCounter.next()

    when (entry is object) or (entry is ref object):
      result = ObjTree(
        styling: sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: true,
        namedFields: true,
        objId: id,
        path: path
      )

    elif isNamedTuple(Obj):
      result = ObjTree(styling: sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: false,
        namedFields: true,
        objId: id,
        path: path
      )

    else:
      result = ObjTree(styling: sconf.getStyling($typeof(entry)),
        kind: okComposed,
        namedFields: false,
        namedObject: false,
        objId: id
      )

    result.path = path

    when (entry is ref object):
      if isNil(entry):
        result = ObjTree(
          styling: sconf.getStyling($typeof(entry)),
          kind: okConstant,
          constType: $(typeof(Obj)),
          strLit: "nil",
          objId: idCounter.next(),
          path: path
        )

      else:
        var idx: int = 0
        for name, value in fieldPairs(entry[]):
          result.fldPairs.add((name, toSimpleTree(
            value, path = path & @[idx],
            sconf = sconf,
            idCounter = idCounter
          )))
          inc idx

    else:
      var idx: int = 0
      for name, value in fieldPairs(entry):
        result.fldPairs.add((name, toSimpleTree(
          value,
          sconf = sconf,
          path = path & objAccs(name),
          idCounter = idCounter,
          conf = conf
        )))
        inc idx


  elif (entry is proc): # proc type
    result = ObjTree(styling: sconf.getStyling($typeof(entry)),
      kind: okConstant,
      constType: $(typeof(Obj)),
      strLit: $(typeof(Obj)),
      path: path,
      objId: idCounter.next()
    )

  elif entry is Option:
    if entry.isSome():
      result = ObjTree(styling: sconf.getStyling($typeof(entry)),
        kind: okComposed,
        name: $typeof(Obj),
        namedObject: false,
        namedFields: false,
        objId: 1, # HACK
        fldPairs: @[(
          name: "val",
          value: toSimpleTree(
            entry.get(), sconf = sconf,
            path = path & @[0], idCounter = idCounter))])

    else:
      result = ObjTree(styling: sconf.getStyling($typeof(entry)),
        kind: okConstant,
        constType: $typeof(Obj),
        strLit: "none",
        path: path,
        objId: idCounter.next()
      )

  else: # everything else
    # echov typeof entry
    var style = sconf.getStyling($typeof(entry))
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
      objId: idCounter.next()
    )



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
    raiseArgumentError(
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
  # if current.annotation.len > 0:
  #   echo current.annotation, " ", current.kind
  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)

  let trySingleLine = (not input.anyOfIt(it.val.multiline())) and
    (not input.anyOfIt(it.annotation.multiline()))

  if trySingleLine:
    var singleLine =
      if current.kind == okSequence or (
        current.kind == okComposed and (not current.namedFields)):
        input.mapIt(&"{it.val.content[0]}").join(conf.seqSeparator)
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

  result = input.enumerate().mapIt(
    relativePosition(it[1].val, (@[makeFldName(it[1])])).
    relativePosition(
      itemLabels,
      ignored = (it[0] == input.len() - 1).tern(
        {rpBottomRight},
        {})))
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
      ).max()

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
                   conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string using configuration `conf`
  var newConf = conf
  newConf.maxWidth = conf.maxWidth - ident
  pstringRecursive(tree, newConf, ident = ident).toString(ident)


const objectPPrintConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  # hideEmptyFields: true # FIXME does not seem to work
)


func getSubnodes*(it: ObjTree): seq[ObjTree] =
  case it.kind:
    of okConstant: raiseArgumentError("No sub for `okConstant`")
    of okSequence: it.valItems
    of okTable: it.valPairs.mapIt(it.val)
    of okComposed: it.fldPairs.mapIt(it.value)


func makeCounter*(): IdCounter = IdCounter()
proc pstring*[Obj](
    obj: Obj, ident: int = 0,
    maxWidth: int = 80,
    sconf: PStyleConf = terminalPStyleConf
  ): string =
  var counter = makeCounter()
  var conf = objectPPrintConf
  conf.maxWidth = maxWidth
  prettyString(toSimpleTree(obj, counter, sconf, conf, @[]), conf, ident)


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
  var counter = makeCounter()
  toSimpleTree(obj, counter, terminalPStyleConf, objectPPrintConf, @[])

proc pprintAtPath*[Obj](obj: Obj, path: TreePath): void =
  var counter = makeCounter()
  pprint toSimpleTree(obj, counter).getAtPath(path)

proc pprint*[Obj](
  obj: Obj, ident: int = 0, maxWidth: int = 80): void =
  echo pstring(obj, ident,  maxWidth)
  # var conf = objectPPrintConf
  # conf.maxWidth = maxWidth
  # echo prettyString(toSimpleTree(obj), conf, ident)

func debugPPrint*[Obj](obj: Obj): void =
  {.noSideEffect.}:
    pprint(obj)
