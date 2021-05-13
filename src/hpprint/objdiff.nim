import hmisc/types/[htrie, colorstring]
import hmisc/other/hshell
import hnimast, hnimast/obj_field_macros
import ../hpprint
import std/[terminal, tables, sequtils]
export hshell

#===========================  type definition  ===========================#

type
  ObjDiffKind* = enum
    odkLen
    odkKind
    odkValue

  ObjDiff* = object
    case kind*: ObjDiffKind:
      of odkLen:
        lhsLen*, rhsLen*: int
      else:
        discard

  TreePath = seq[int]
  ObjDiffPaths = Trie[int, ObjDiff]

#=========================  diff implementation  =========================#


proc pprintCwdiff*[T](lhs, rhs: T): void =
  "/tmp/generated.nim".writeFile(pstring lhs)
  "/tmp/expected.nim".writeFile(pstring rhs)
  execShell(shellCmd(cwdiff, "/tmp/expected.nim", "/tmp/generated.nim"))

proc diff*[T](lhsIn, rhsIn: T, path: TreePath = @[0]): ObjDiffPaths = #[

TODO sort fields in unordered associative containers. Otherwise it
     will be impossible to adequately check what is missing.

IDEA provide LCS-based difference reports for sequences (if possible)
     instead of simply comparing lengths

FIXME cannot deal with `Type = HashTable[...]`

]#

  when (T is seq) or (T is array):
    if lhsIn.len() != rhsIn.len():
      result[path] = ObjDiff(kind: odkLen,
                             lhsLen: lhsIn.len(), rhsLen: rhsIn.len())

    for idx, (lval, rval) in zip(lhsIn, rhsIn):
      result.merge diff(lval, rval, path & @[idx])
  elif (T is Table): # TODO implement table diffing
    # static: echo typeof T
    let
      lhsPairs = lhsIn.mapPairs(($lhs, rhs)).withResIt:
        it.sortedByIt(it[0])

      rhsPairs = rhsIn.mapPairs(($lhs, rhs)).withResIt:
        it.sortedByIt(it[0])

    for (lhs, rhs) in zip(lhsPairs, rhsPairs):
      echo lhsPairs
      echo rhsPairs

  elif (T is object) or (T is tuple) or (T is ref object):
    mixin parallelFieldPairs
    parallelFieldPairs(lhsIn, rhsIn):
      when isKind:
        if lhs != rhs:
          result[path] = ObjDiff(kind: odkKind)
      else:
        result.merge diff(lhs, rhs, path & @[valIdx])
  # elif T is tuple:
  #   var idx = 0
  #   for lhs, rhs in fieldPairs(lhsIn, rhsIn):

  #     inc idx
  else:
    # static: echo typeof(T)
    if lhsIn != rhsIn:
      result[path] = ObjDiff(kind: odkValue)

#========================  diff pretty-printing  =========================#

func getAtPath*(tree: ObjTree, path: TreePath): (ObjTree, seq[ObjAccessor]) =
  case tree.kind:
    of okComposed:
      if path.len <= 1:
        return (tree, @[ObjAccessor(kind: okConstant)])
      else:
        let (subtree, accs) = getAtPath(tree.fldPairs[path[1]].value, path[1..^1])
        return (subtree, @[
          ObjAccessor(kind: okComposed, name: tree.fldPairs[path[1]].name)
        ] & accs)
    of okConstant:
      return (tree, @[ObjAccessor(kind: okConstant)])
    of okSequence:
      if path.len <= 1: # Get sequence itself
        return (tree, @[
          ObjAccessor(kind: okConstant)
        ])
      else: # Get element from sequence
        let (subtree, accs) = getAtPath(tree.valItems[path[1]], path[1..^1])
        return (subtree, @[
          ObjAccessor(kind: okSequence, idx: path[1])
        ] & accs)
    of okTable:
      let (subtree, accs) = getAtPath(tree.valPairs[path[1]].val, path[1..^1])
      return (subtree, @[
        ObjAccessor(kind: okTable, key: tree.valPairs[path[1]].key)
      ])


func mgetAtPath*(tree: var ObjTree, path: TreePath): var ObjTree =
  case tree.kind:
    of okComposed:
      if path.len <= 1:
        return tree
      else:
        return mgetAtPath(tree.fldPairs[path[1]].value, path[1..^1])
    of okConstant:
      # debugecho tree
      return tree
    of okSequence:
      if path.len <= 1:
        return tree
      else:
        return mgetAtPath(tree.valItems[path[1]], path[1..^1])
    of okTable:
      return mgetAtPath(tree.valPairs[path[1]].val, path[1..^1])

func toStr*(accs: seq[ObjAccessor]): string =
  for acc in accs:
    case acc.kind:
      of okComposed:
        result &= "." & acc.name
      of okTable:
        result &= "[" & acc.key & "]"
      of okSequence:
        result &= "[" & $acc.idx & "]"
      of okConstant:
        discard

proc ppDiff*[T](
    lhs, rhs: T,
    printFull: bool = false,
    die: bool = false,
    maxWidth: int = 80
  ): void =
  ## Pretty-print difference between two objects
  # TODO print items side-by-side if possible
  let diffpaths = diff(lhs, rhs)
  if diffpaths.paths().len == 0:
    return

  var
    lhsObjTree = toObjTree(lhs)
    rhsObjTree = toObjTree(rhs)

  for path in diffpaths.paths():
    let (lhsTree, lhsNamePath) = getAtPath(lhsObjTree, path)
    let (rhsTree, rhsNamePath) = getAtPath(rhsObjTree, path)
    let diff: ObjDiff = diffpaths[path]
    # echo path
    case diff.kind:
      of odkLen:
        if diff.lhsLen < diff.rhsLen:
          lhsObjTree.mgetAtPath(path).annotate(
            " # Fever elements in LHS".toYellow({styleItalic}))
          for idx in diff.lhsLen ..< diff.rhsLen:
            var tree = mgetAtPath(rhsObjTree, path & @[idx])
            tree.styling.fg = fgGreen
            lhsObjTree.mgetAtPath(path).valItems.add tree
        else:
          lhsObjTree.mgetAtPath(path).annotate(
            " # More elements in LHS".toYellow({styleItalic}))
          for idx in diff.rhsLen ..< diff.lhsLen:
            lhsObjTree.mgetAtPath(path & @[idx]).styling.fg = fgRed
      else:
        lhsObjTree.mgetAtPath(path).annotate(
          " " & rhsTree.pstring().toRed() &
            " # Value mismatch".toYellow({styleItalic}))
        lhsObjTree.mgetAtPath(path).styling.fg = fgGreen

  let str = pstring(lhsObjTree, maxWidth = maxWidth)
  when defined(plainStdout):
    echo str.stripSGR()

  else:
    echo str
    # pprint lhsObjTree, maxWidth = maxWidth

  if die and diffpaths.paths().len > 0:
    raiseAssert("Difference between objects")

proc assertNoDiff*[T](lhs, rhs: T): void =
  if lhs != rhs:
    ppDiff(lhs, rhs, die = true)
