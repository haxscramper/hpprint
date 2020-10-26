import hasts/[graphviz_ast, html_ast]
import hdrawing, hdrawing/[term_buf, block_grid]

import hmisc/algo/htree_mapping
import hmisc/helpers
import hnimast
import hmisc/types/[seq2d, hvariant]

export hvariant
import ../hpprint
import sequtils

func toTermBuf*(obj: ObjElem[TermTextConf]): TermBuf = toTermBuf(obj.text)

type
  DotGenConfig = object
    f1: int

  GridConvConfig = object
    ## Configuration for converting `ObjTree` into `BlockGrid`
    retainDisabledFields: bool

  GridConvRole = enum
    gcrTypeName
    gcrFieldName
    gcrFieldValue
    gcrObjName
    gcrKeyValue
    gcrSeqIndex
    gcrConstValue

  GridPlacementRole = enum
    gprInlined
    gprEmbedded
    gprStandalone

  GridConvStage = object
    ## Description of current tree -> grid conversion stage
    role: GridConvRole
    placement: GridPlacementRole
    fldDisabled: bool
    isPointedTo: bool

func makeConvStage*(role: GridConvRole, placement: GridPlacementRole): GridConvStage =
  GridConvStage(role: role, placement: placement)

type
  ObjGrid[Conf] = BlockGrid[ObjElem[Conf]]
  ObjCell[Conf] = GridCell[ObjElem[Conf]]
  ObjRow[Conf] = BlockGridRow[ObjElem[Conf]]
  ConfGenProc[Conf] = proc(
    obj: ObjTree,
    config: GridConvConfig,
    stage: GridConvStage): Conf

func toCells*[Conf](
  obj: ObjTree,
  makeConf: ConfGenProc[Conf]): tuple[ctype, value: ObjCell[Conf]] =
  ## Convert constant to row
  assert obj.kind == okConstant
  return (
    makeCell(makeObjElem[Conf](
      obj.constType,
      makeConf(
        obj,
        GridConvConfig(), #[ IMPLEMENT ]#
        makeConvStage(gcrTypeName, gprInlined)
      )
    )),
    makeCell(makeObjElem[Conf](
      obj.strLit,
      makeConf(
        obj,
        GridConvConfig(), #[ IMPLEMENT ]#
        makeConvStage(gcrConstValue, gprInlined)
      )
    )),
  )

proc toGrid*[Conf](obj: ObjTree, makeConf: ConfGenProc[Conf]): tuple[
  grid: BlockGrid[ObjElem[Conf]],
  edges: seq[tuple[
    pholder: ObjElem[Conf],
    ejected: ObjTree
  ]]] =
  ## Convert object tree into grid. Return resulting grid and all
  ## 'ejected' subtrees.
  # TODO DOC
  case obj.kind:
    of okConstant:
      let (ctype, value) = toCells[Conf](obj, makeConf)
      result.grid = makeGrid[ObjElem[Conf]](
        @[@[ctype, value]], makeThinLineGridBorders())
    of okSequence: # Sequence
      discard
    of okTable: # Table
      discard
    of okComposed:
      if obj.namedObject: # Object
        discard
      else:
        if obj.namedFields: # Named tuple
          discard
        else: # Anon. tuple
          result.grid = makeGrid[ObjElem[Conf]](
            makeCell(makeObjElem[Conf](
              obj.name & "+",
              makeConf(
                obj,
                GridConvConfig(), #[ IMPLEMENT ]#
                makeConvStage(gcrObjName, gprInlined)
              )
            ), (2, 1)),
            makeThinLineGridBorders()
          )

          for (name, value) in obj.fldPairs:
            case value.kind:
              of okConstant:
                let (ctype, value) = toCells[Conf](value, makeConf)
                result.grid.appendRow(@[ctype, value])

              else:
                discard


proc makeTermGridConf(
  obj: ObjTree,
  config: GridConvConfig,
  stage: GridConvStage): TermTextConf =
  discard
  # echo "Making term text conf, role: ", stage.role

proc toPGrid*[T](obj: T): string =
  var counter = makeCounter()
  var sconf = PStyleConf()
  let tree = toSimpleTree(obj, counter, sconf)
  let (grid, edges) = toGrid[TermTextConf](tree, makeTermGridConf)
  return grid.toTermBuf().toString()

proc toHTML*(grid: BlockGrid[StrBlock]): HtmlElem =
  result = HtmlElem(kind: hekTable, border: 1)
  for (pos, cell) in grid.iterSomeCells():
    case cell.isItem:
      of true:
        result.setOrAddCell(
          pos,
          withIt(cell.item.joinl().toHtmlCell()) do:
            it.cellSize = cell.size()
        )
      of false:
        result.setOrAddCell(
          pos,
          withIt(cell.grid.toHTML().toHtmlCell()) do:
            it.cellSize = cell.size()
        )

# proc foldObject(obj: ObjTree): tuple[node: Node, edges: seq[Edge]] =
#   ##[

# Recurisvely convert `ObjTree` into graphviz html-like node.

# All primitive subitems are embedded into resulting node. All other
# elements as converted into edges.

#   ]##
#   let (grid, edges) = obj.toGrid(obj.objId)
#   result.node = Node(
#     shape: nsaPlaintext,
#     id: obj.objId,
#     htmlLabel: grid.toTable()
#   )

#   for (src, to) in edges:
#     result.edges.add Edge(
#       src: src,
#       to: @[ to ]
#     )

# proc toDotGraph*[Obj](obj: Obj, conf: DotGenConfig = DotGenConfig()): Graph =
#   var counter =
#     iterator(): int {.closure.} =
#       var cnt: int = 0
#       while true:
#         yield cnt
#         inc cnt

#   let tree = toSimpleTree(obj, counter)
#   # TO whoever reading this: I had to use life support system to not
#   # die of brain overload. Just saying.
#   var folded = tree.mapItDFS(
#     outType = seq[Var2[Edge, Node]],
#     hasSubnodes = (it.kind != okConstant),
#     subnodeCall = it.getSubnodes(),
#     op =
#       block:
#         let (node, edges) = it.foldObject()
#         @[ toVar2[Edge, Node](node) ] &
#           toVar2[Edge, Node](edges) &
#           sequtils.concat(subt)
#   )

#   result = Graph(
#     nodes: folded.filterIt(it.hasType(Node)).mapIt(it.get(Node)),
#     edges: folded.filterIt(it.hastype(Edge)).mapIt(it.get(Edge))
#   )
