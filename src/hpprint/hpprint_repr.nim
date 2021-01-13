import strutils, sequtils, strformat, sugar
import hmisc/types/colorstring
import hnimast
import hmisc/algo/[halgorithm, hseq_mapping, clformat]

export ObjTree

func pptConst*(
  val: string, styling: PrintStyling = initPrintStyling()): ObjTree =
  ObjTree(styling: styling, kind: okConstant, strlit: val)

func pptSeq*(vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: initPrintStyling(),
          kind: okSequence, valItems: toSeq(vals))

func pptSeq*(styling: PrintStyling = initPrintStyling(),
             vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: styling,
          kind: okSequence, valItems: toSeq(vals))

func pptSeq*(valType: string,
             vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: initPrintStyling(),
          kind: okSequence, valItems: toSeq(vals), itemType: valType)


func pptSeq*(valType: string,
             styling: PrintStyling,
             vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: styling,
          kind: okSequence, valItems: toSeq(vals), itemType: valType)


func pptMap*(kvTypes: (string, string),
             styling: PrintStyling,
             vals: varargs[tuple[
               key: string,
               val: ObjTree]]): ObjTree =
  ObjTree(
    styling: styling,
    kind: okTable,
    keyStyling: initPrintStyling(),
    keyType: kvTypes[0],
    valType: kvTypes[1],
    valPairs: toSeq(vals)
  )


func pptMap*(kvTypes: (string, string),
             vals: varargs[tuple[key: string, val: ObjTree]]): ObjTree =
  pptMap(kvTypes, initPrintStyling(), vals)

func pptObj*(name: string,
             styling: PrintStyling,
             flds: varargs[tuple[
               name: string,
               value: ObjTree]]): ObjTree =

  ObjTree(
    styling: styling,
    kind: okComposed,
    namedObject: true,
    namedFields: true,
    name: name,
    fldPairs: toSeq(flds)
  )


func pptObj*(name: string,
             flds: varargs[tuple[name: string, value: ObjTree]]): ObjTree =
  pptObj(name, initPrintStyling(), flds)

func pptObj*(
  name: string, styling: PrintStyling, flds: varargs[ObjTree]): ObjTree =
  result = ObjTree(
    styling: styling,
    kind: okComposed,
    namedObject: true,
    namedFields: false,
    name: name
  )

  for fld in flds:
    result.fldPairs.add(("", fld))

func pptObj*(name: string, flds: varargs[ObjTree]): ObjTree =
  pptObj(name, initPrintStyling(), flds)


type
  TreeReprParams* = object
    maxDepth: int
    outWidth: int
    newlines: bool

#==============================  Lisp repr  ==============================#

func lispReprImpl*(tree: ObjTree,
                   params: TreeReprParams,
                   level: int): string =
  # TODO implement basic layout configuration
  if level >= params.maxDepth:
    return "..."

  let sep = params.newlines.tern("\n", " ")
  let prefix = params.newlines.tern("  ".repeat(level + 1), "")
  case tree.kind:
    of okConstant:
      return tree.strLit.toStyled(tree.styling)
    of okSequence:
      return (block:
        collect(newSeq):
          for it in tree.valItems:
            lispReprImpl(it, params, level + 1)
        ).joinw().wrap(("'(", ")"))
    of okTable:
      return tree.valPairs.
        mapPairs(fmt("(:{lhs} {rhs.lispReprImpl(params, level)})")).
        join(sep).wrap("()")
    of okComposed:
      return (params.newlines.tern(sep, "") & tree.fldPairs.
        mapPairs(
          prefix & tree.namedFields.tern(&":{lhs} ", "") &
          rhs.lispReprImpl(params, level + 1)).
        join(sep)).
        wrap do:
          if tree.namedObject:
            if tree.name.validIdentifier():
              (&"({tree.name.toStyled(tree.styling)} ", ")")
            else:
              (&"(`{tree.name.toStyled(tree.styling)}` ", ")")
          else:
            (("(", ")"))


func lispRepr*(
  tree: ObjTree, maxlevel: int = 60, newlines: bool = false): string =
  lispReprImpl(tree, TreeReprParams(
    maxDepth: maxlevel,
    newlines: newlines
  ), level = 0)

#==============================  Tree repr  ==============================#
func treeReprImpl*(
    tree: ObjTree,
    params: TreeReprParams,
    pref: seq[bool],
    parentMaxIdx, currIdx: int,
    parentKind: ObjKind,
    backticks: bool
  ): seq[string] =

  let arrow =
    case parentKind:
      of okComposed: "+-> "
      of okConstant: "+-> "
      of okSequence: "+-- "
      of okTable: "+-: "

  let prefStr =
    if pref.len > 0:
      if parentKind == okSequence and pref.len == 1:
        arrow
      else:
        pref.mapIt(it.tern("|   ", "    ")).join("") & arrow
    else:
      ""

  let prefStrNoarrow = pref.mapIt(it.tern("|   ", "    ")).join("")
  case tree.kind:
    of okConstant:
      var idx = 0
      for line in tree.strLit.split('\n'):
        result.add tern(
          idx == 0,
          prefStr,
          pref.mapIt(tern(it, "|    ", "    ")).join("") &
            repeat(" " , arrow.len)
        ) & line.toStyled(tree.styling)
        inc idx

    of okSequence:
      if pref.len + 1 > params.maxdepth:
        return @[prefStr & (tree.itemType.len > 0).tern(
          &"seq[{tree.itemType}] ", "") & "... (" &
          toPluralNoun("item", tree.valItems.len) & ")"
        ]

      for idx, item in tree.valItems:
        result &= treeReprImpl(
          item,
          params,
          pref & @[currIdx != parentMaxIdx],
          parentMaxIdx = tree.valItems.len - 1,
          currIdx = idx,
          parentKind = tree.kind,
          backticks = backticks
        )

    of okTable:
      let name = (tree.keyType.len > 0 and tree.valType.len > 0).tern(
          &"[{tree.keyType} -> {tree.valType}] ", "")

      if pref.len + 1 > params.maxdepth:
        result &= prefStr & name & "... (" &
          toPluralNoun("pair", tree.valPairs.len) & ")"
        return

      else:
        result &= prefStr & name

      result &= concat mapPairs(tree.valPairs) do:
         @[prefStrNoarrow & (currIdx < parentMaxIdx).tern("|", " ") &
           "   +-: " & lhs] &
         treeReprImpl(
           rhs,
           params,
           pref & @[currIdx != parentMaxIdx] &
             (rhs.kind == okConstant).tern(
               @[idx < tree.valPairs.len - 1], @[]),
           parentMaxIdx = tree.valPairs.len - 1,
           currIdx = idx,
           parentKind = rhs.kind,
           backticks = backticks
         )

    of okComposed:
      let
        treeName = tree.name.toStyled(tree.styling)
        name = tern(
          tree.name.validIdentifier or not backticks,
          treeName, treeName.wrap("``"))

      if pref.len + 1 > params.maxdepth:
        result &= prefStr & name & " ... (" &
          toPluralNoun("field", tree.fldPairs.len) & ")"
        return

      elif
        (tree.fldPairs.len == 1) and
        (tree.fldPairs[0].value.kind == okConstant):

        let fld = tree.fldPairs[0]
        if fld.value.kind == okConstant:
          if tree.namedFields:
            result &= &"{prefStr}{name} {fld.value}"
          else:
            result &= &"{prefStr}{name} +-> {fld.value.strLit}"

          return

      else:
        result &= prefStr & name & ":"

      result &= concat mapPairs(tree.fldPairs) do:
        tree.namedFields.tern(@["  " & prefStr & lhs], @[]) &
        treeReprImpl(
          rhs,
          params,
          pref & @[currIdx != parentMaxIdx],
          parentMaxIdx = tree.fldPairs.len - 1,
          currIdx = idx,
          parentKind = tree.kind,
          backticks = backticks
        )

func treeRepr*(
  tree: ObjTree, maxlevel: int = 60, backticks: bool = true): string =
  treeReprImpl(
    tree,
    TreeReprParams(
      maxDepth: maxlevel
    ),
    @[], 0, 0,
    parentKind = tree.kind,
    backticks = backticks
  ).join("\n")
