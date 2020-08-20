import strutils, sequtils, strformat, sugar
import hmisc/types/[hnim_ast, colorstring]
import hmisc/algo/[halgorithm, hseq_mapping, clformat]

func pptConst*(val: string): ObjTree =
  ObjTree(styling: initPrintStyling(), kind: okConstant, strlit: val)

func pptSeq*(vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: initPrintStyling(), kind: okSequence, valItems: toSeq(vals))

func pptSeq*(valType: string, vals: varargs[ObjTree]): ObjTree =
  ObjTree(styling: initPrintStyling(), kind: okSequence, valItems: toSeq(vals), itemType: valType)

func pptMap*(kvTypes: (string, string),
             vals: varargs[tuple[
               key: string,
               val: ObjTree]]): ObjTree =

  ObjTree(styling: initPrintStyling(), kind: okTable,
             keyType: kvTypes[0],
             valType: kvTypes[1],
             valPairs: toSeq(vals))

func pptObj*(name: string,
             flds: varargs[tuple[
               name: string,
               value: ObjTree]]): ObjTree =

  ObjTree(
    styling: initPrintStyling(),
    kind: okComposed,
    namedObject: true,
    namedFields: true,
    name: name,
    fldPairs: toSeq(flds)
  )

func pptObj*(name: string, flds: varargs[ObjTree]): ObjTree =
  result = ObjTree(
    styling: initPrintStyling(),
    kind: okComposed,
    namedObject: true,
    namedFields: false,
    name: name# ,
             # fldPairs: flds.mapIt(("", it))
  )

  for fld in flds:
    result.fldPairs.add(("", fld))


type
  TreeReprParams* = object
    maxDepth: int
    outWidth: int
    newlines: bool

#==============================  Lisp repr  ==============================#

func lispReprImpl*(tree: ObjTree,
                   params: TreeReprParams,
                   level: int): string =

  if level >= params.maxDepth:
    return "..."

  let sep = params.newlines.tern("\n", " ")
  let prefix = params.newlines.tern("  ".repeat(level + 1), "")
  case tree.kind:
    of okConstant:
      return tree.strLit
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
              (&"({tree.name} ", ")")
            else:
              (&"(`{tree.name}` ", ")")
          else:
            (("(", ")"))


func lispRepr*(
  tree: ObjTree, maxlevel: int = 60, newlines: bool = false): string =
  lispReprImpl(tree, TreeReprParams(
    maxDepth: maxlevel,
    newlines: newlines
  ), level = 0)

#==============================  Tree repr  ==============================#
func treeReprImpl*(tree: ObjTree,
                   params: TreeReprParams,
                   pref: seq[bool],
                   parentMaxIdx, currIdx: int,
                   parentKind: ObjKind): seq[string] =

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
      return @[prefStr & tree.strLit]
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
          parentKind = tree.kind
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
           parentKind = rhs.kind
         )
    of okComposed:
      let
        name = tree.name.validIdentifier.tern(
          tree.name, tree.name.wrap("``"))

      if pref.len + 1 > params.maxdepth:
        result &= prefStr & name & " ... (" &
          toPluralNoun("field", tree.fldPairs.len) & ")"
        return
      elif #[ FIXME ]# false and
        (tree.fldPairs.len == 1) and
        (tree.fldPairs[0].value.kind == okConstant):
        let fld = tree.fldPairs[0]
        if fld.value.kind == okConstant:
          if tree.namedFields:
            result &= &" {fld.name} {fld.value}" # TODO handle multiline
          else:
            result &= &" {fld.value}" # TODO handle multiline

          return
      else:
        result &= prefStr & name & ":"

      result &= concat mapPairs(tree.fldPairs) do:
        tree.namedFields.tern(@[prefStr & lhs], @[]) &
        treeReprImpl(
          rhs,
          params,
          pref & @[currIdx != parentMaxIdx],
          parentMaxIdx = tree.fldPairs.len - 1,
          currIdx = idx,
          parentKind = tree.kind
        )

func treeRepr*(tree: ObjTree, maxlevel: int = 60): string =
  treeReprImpl(
    tree,
    TreeReprParams(
      maxDepth: maxlevel
    ),
    @[], 0, 0,
    parentKind = tree.kind
  ).join("\n")
