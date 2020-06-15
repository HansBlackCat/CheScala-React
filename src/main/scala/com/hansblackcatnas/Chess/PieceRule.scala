package com.hansblackcat.Chess
import scala.collection.mutable.{Map => MMap, ArrayBuffer}
import scala.math._

class PieceRule(currentBoard: MMap[String, Info]) extends Root {

  private[this] var _currentBoard = currentBoard

  // match infoNone first,,
  // run except King -- may cause check when move

  // opt Array[ExLocation]
  // ipt Info // iter == for i
  private[this] def baseRangeExceptKing(
    location: String,
    kind: PGNPieceKind,
    isWhite: Boolean,
    currentBoard: MMap[String, Info]
  ) = {
    require(baseGridKeys contains location)

    val locArr = ExLocation(location).toArrLoc

    // Each pieces' locations
    val availKeysArr = (for {
      i <- currentBoard
      if (
        i._2 match {
          case InfoNone => false
          case _        => if (i._1 == location) false else true
        }
      )
    } yield ExLocation(i._1)).toArray

    val kindMatch = {
      val exceptKing = kind match {
        case King =>
          val t =
            for (i <- -1 to 1; j <- -1 to 1 if (i, j) != (0, 0))
              yield locArr +> (i, j)
          t
        case Queen =>
          val t1 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, 0)
          val t2 = for (j <- -8 to 8 if j != 0) yield locArr +> (0, j)
          val t3 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, i)
          val t4 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, -i)
          t1 ++ t2 ++ t3 ++ t4
        case Rook =>
          val t1 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, 0)
          val t2 = for (j <- -8 to 8 if j != 0) yield locArr +> (0, j)
          t1 ++ t2
        case Bishop =>
          val t3 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, i)
          val t4 = for (i <- -8 to 8 if i != 0) yield locArr +> (i, -i)
          t3 ++ t4
        case Knight =>
          val t =
            for (i <- -2 to 2; j <- -2 to 2 if abs(i) + abs(j) == 3)
              yield locArr +> (i, j)
          t
        case Pawn =>
          if (isWhite) {
            val default = IndexedSeq(locArr +> (0, 1))
            val doubleMove =
              if (locArr(1) == 1) IndexedSeq(locArr +> (0, 2)) else IndexedSeq()
            // En Passant
            val enPassant = {
              val initFoeMaybePawnLoc = Array(
                (locArr +> (1, 0)),
                (locArr +> (-1, 0))
              ).filter(i => 0 <= i(0) && i(0) < 8 && 1 < i(1) && i(1) < 6)
              for {
                i <- initFoeMaybePawnLoc;
                if (
                  currentBoard(i.toExLoc().location) match {
                    case InfoNone        => false
                    case InfoWhite(_, _) => false
                    case InfoBlack(kind, init) =>
                      if (kind == Pawn && init == true) true else false
                  }
                )
              } yield (i +> (0, 1))
            }
            default ++ enPassant ++ doubleMove
          } else {
            val default = IndexedSeq(locArr +> (0, -1))
            val doubleMove =
              if (locArr(1) == 6) IndexedSeq(locArr +> (0, -2))
              else IndexedSeq()
            val enPassant = {
              val initFoeMaybePawnLoc = Array(
                (locArr +> (1, 0)),
                (locArr +> (-1, 0))
              ).filter(i => 0 <= i(0) && i(0) < 8 && 1 < i(1) && i(1) < 6)
              for {
                i <- initFoeMaybePawnLoc;
                if (
                  currentBoard(i.toExLoc().location) match {
                    case InfoNone        => false
                    case InfoBlack(_, _) => false
                    case InfoWhite(kind, init) =>
                      if (kind == Pawn && init == true) true else false
                  }
                )
              } yield (i +> (0, 1))
            }
            default ++ enPassant ++ doubleMove
          }
        case _ =>
          IndexedSeq[Array[Int]]()
      }
      (for (i <- exceptKing.filter(i =>
              0 <= i(0) && i(0) < 8 && 0 <= i(1) && i(1) < 8
            )) yield (i(0) + 1) #> (i(1) + 1)).toArray
    }
    val allyfoeMatch = {
      val intersec = kindMatch intersect availKeysArr
      val diff = kindMatch diff availKeysArr

      var tmpBuffer = ArrayBuffer[Array[Int]]()
      for (i <- intersec) {
        val unit = (i.toArrLoc -> locArr).toUnit()
        val unitMul = for (j <- 0 to 7) yield (i.toArrLoc +> unit.map(_ * j))

        for (k <- unitMul if 0 <= k(0) && k(0) < 8 && 0 <= k(1) && k(1) < 8) {
          tmpBuffer = k +: tmpBuffer
        }
      }
      val dupArr = tmpBuffer.map(_.toExLoc())

      isWhite match {
        case true =>
          for (i <- intersec) {
            currentBoard(i.location) match {
              case InfoNone =>
                throw new Exception("somethings go wrong with foe-ally match")
              case InfoBlack(kind, init) => { dupArr -= i }
              case InfoWhite(kind, init) => {}
            }
          }
        case false =>
          for (i <- intersec) {
            currentBoard(i.location) match {
              case InfoNone =>
                throw new Exception("somethings go wrong with foe-ally match")
              case InfoWhite(kind, init) => { dupArr -= i }
              case InfoBlack(kind, init) => {}
            }
          }

      }

      dupArr.toArray
    }
    val pawnSpecial = {
      if (kind == Pawn) {
        isWhite match {
          case true =>
            Array(locArr +> (1, 1), locArr +> (-1, 1))
              .filter(i => 0 <= i(0) && i(0) < 8 && 0 <= i(1) && i(1) < 8)
              .filter(j =>
                currentBoard(j.toExLoc().location) match {
                  case InfoBlack(kind, init) => true
                  case _                     => false
                }
              )
              .map(_.toExLoc())
          case false =>
            Array(locArr +> (1, -1), locArr +> (-1, -1))
              .filter(i => 0 <= i(0) && i(0) < 8 && 0 <= i(1) && i(1) < 8)
              .filter(j =>
                currentBoard(j.toExLoc().location) match {
                  case InfoWhite(kind, init) => true
                  case _                     => false
                }
              )
              .map(_.toExLoc())
        }
      } else Array[ExLocation]()
    }
    val pawnCantCatchFront = {
      if (kind == Pawn) {
        isWhite match {
          case true =>
            Array(locArr +> (0, 1), locArr +> (0, 2))
              .filter(i => 0 <= i(0) && i(0) < 8 && 0 <= i(1) && i(1) < 8)
              .filter(j =>
                currentBoard(j.toExLoc().location) match {
                  case InfoBlack(kind, init) => true
                  case _                     => false
                }
              )
              .map(_.toExLoc())
          case false =>
            Array(locArr +> (0, -1), locArr +> (0, -2))
              .filter(i => 0 <= i(0) && i(0) < 8 && 0 <= i(1) && i(1) < 8)
              .filter(j =>
                currentBoard(j.toExLoc().location) match {
                  case InfoWhite(kind, init) => true
                  case _                     => false
                }
              )
              .map(_.toExLoc())
        }
      } else Array[ExLocation]()
    }
    kindMatch diff allyfoeMatch concat pawnSpecial diff pawnCantCatchFront
  }

  private[this] def _rangeFinder(ipt: MMap[String, Info]) = {
    val _currentBoard = ipt
    var rangeFinderMMap = MMap[String, Array[ExLocation]]()
    var forKingRangeW = MMap[String, Array[ExLocation]]()
    var forKingRangeB = MMap[String, Array[ExLocation]]()

    for (i <- _currentBoard) {
      i._2 match {
        case InfoNone => rangeFinderMMap(i._1) = Array[ExLocation]()
        case InfoBlack(kind, init) =>
          val _location = i._1
          val _kind = kind
          val _isWhite = false

          rangeFinderMMap(i._1) =
            baseRangeExceptKing(_location, _kind, _isWhite, _currentBoard)
          forKingRangeB(i._1) =
            baseRangeExceptKing(_location, _kind, _isWhite, _currentBoard)
        case InfoWhite(kind, init) =>
          val _location = i._1
          val _kind = kind
          val _isWhite = true

          rangeFinderMMap(i._1) =
            baseRangeExceptKing(_location, _kind, _isWhite, _currentBoard)
          forKingRangeW(i._1) =
            baseRangeExceptKing(_location, _kind, _isWhite, _currentBoard)
      }
    }
    (rangeFinderMMap, forKingRangeB, forKingRangeW)
  }

  def rangeFinder = {
    var (rangeFinderMMap, forKingRangeB, forKingRangeW) = _rangeFinder(
      _currentBoard
    )
    for (i <- _currentBoard) {
      i._2 match {
        // ----------------------------------------------------------------------------
        // Black King
        // ----------------------------------------------------------------------------
        case InfoBlack(King, init) =>
          val kingPossible = rangeFinderMMap(i._1)
          var whenMoveCheck = Array[ExLocation]()
          var castlingPosition = Array[ExLocation]()
          for {
            // All valid enemies near King
            j <- kingPossible;
            if (
              _currentBoard(j.location) match {
                case InfoWhite(kind, init) => true
                case _                     => false
              }
            )
          } {
            val copyCurrent = _currentBoard.clone()
            copyCurrent(j.location) = InfoBlack(Pawn, false)
            val copyWhite = _rangeFinder(copyCurrent)._3

            for (k <- copyWhite) {
              if (k._2 contains j) whenMoveCheck = j +: whenMoveCheck
            }
          }
          // WPawn Diag
          for (x <- _currentBoard) {
            x._2 match {
              case InfoWhite(Pawn, init) => 
                val tmpSave = forKingRangeW(x._1)
                ExLocation(x._1).toArrLoc(0) match {
                  case a if (a<1) => 
                    forKingRangeW(x._1) = tmpSave ++ Array(ExLocation(x._1) + (1,1))
                  case a if (a>6) =>
                    forKingRangeW(x._1) = tmpSave ++ Array(ExLocation(x._1) + (-1,1))
                  case c          =>
                    forKingRangeW(x._1) = tmpSave ++ Array(ExLocation(x._1) + (1,1), ExLocation(x._1) + (-1,1))
                }
              case _ => {}
            }
          }
          // estimate king checked position
          for (l <- forKingRangeW) {
            for (m <- l._2) {
              if (kingPossible contains m) whenMoveCheck = m +: whenMoveCheck
            }
          }
          // ----------------------------------------------------------------------------
          // BK Castling
          // ----------------------------------------------------------------------------
          val castlingPositionA = {
            var castlingPositionABool = true
            if (init == true &&
                (
                  _currentBoard("a8") match {
                    case InfoBlack(kind, init) =>
                      if (kind == Rook && init == true) true else false
                    case _ => false
                  }
                ) && {
                  for (i <- forKingRangeW) {
                    i._2 match {
                      case a if a contains ExLocation("c8") =>
                        castlingPositionABool = false
                      case a if a contains ExLocation("d8") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("e8") =>
                        castlingPositionABool = false
                      case _ => {}
                    }
                  }
                  castlingPositionABool
                } && {
                  _currentBoard("b8") == InfoNone && _currentBoard("c8") == InfoNone && _currentBoard(
                    "d8"
                  ) == InfoNone
                }) castlingPosition = ExLocation("c8") +: castlingPosition
          }
          val castlingPositionH = {
            var castlingPositionABool = true
            if (init == true &&
                (
                  _currentBoard("h8") match {
                    case InfoBlack(kind, init) =>
                      if (kind == Rook && init == true) true else false
                    case _ => false
                  }
                ) && {
                  for (i <- forKingRangeW) {
                    i._2 match {
                      case a if a contains ExLocation("g8") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("e8") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("f8") =>
                        castlingPositionABool = false
                      case _ => {}
                    }
                  }
                  castlingPositionABool
                } && {
                  _currentBoard("f8") == InfoNone && _currentBoard("g8") == InfoNone
                }) castlingPosition = ExLocation("g8") +: castlingPosition
          }
          rangeFinderMMap(i._1) =
            (rangeFinderMMap(i._1) diff whenMoveCheck.distinct) ++ castlingPosition
        // ----------------------------------------------------------------------------
        // White King
        // ----------------------------------------------------------------------------
        case InfoWhite(King, init) => {}
          val kingPossible = rangeFinderMMap(i._1)
          var whenMoveCheck = Array[ExLocation]()
          var castlingPosition = Array[ExLocation]()
          for {
            // Valid enemies near King
            j <- kingPossible;
            if (
              _currentBoard(j.location) match {
                case InfoBlack(kind, init) => true
                case _                     => false
              }
            )
          } {
            val copyCurrent = _currentBoard.clone()
            copyCurrent(j.location) = InfoWhite(Pawn, false)
            val copyBlack = _rangeFinder(copyCurrent)._2

            for (k <- copyBlack) {
              if (k._2 contains j) whenMoveCheck = j +: whenMoveCheck
            }
          }
          // BPawn Diag
          for (x <- _currentBoard) {
            x._2 match {
              case InfoBlack(Pawn, init) => 
                val tmpSave = forKingRangeB(x._1)
                ExLocation(x._1).toArrLoc(0) match {
                  case a if (a<1) => 
                    forKingRangeB(x._1) = tmpSave ++ Array(ExLocation(x._1) + (1,-1))
                  case a if (a>6) =>
                    forKingRangeB(x._1) = tmpSave ++ Array(ExLocation(x._1) + (-1,-1))
                  case c          =>
                    forKingRangeB(x._1) = tmpSave ++ Array(ExLocation(x._1) + (1,-1), ExLocation(x._1) + (-1,-1))
                }
              case _ => {}
            }
          }
          for (l <- forKingRangeB) {
            for (m <- l._2) {
              if (kingPossible contains m) whenMoveCheck = m +: whenMoveCheck
            }
          }
          // ----------------------------------------------------------------------------
          // WK Castling
          // ----------------------------------------------------------------------------
          val castlingPositionA = {
            var castlingPositionABool = true
            if (init == true &&
                (
                  _currentBoard("a1") match {
                    case InfoWhite(kind, init) =>
                      if (kind == Rook && init == true) true else false
                    case _ => false
                  }
                ) && {
                  for (i <- forKingRangeB) {
                    i._2 match {
                      case a if a contains ExLocation("c1") =>
                        castlingPositionABool = false
                      case a if a contains ExLocation("d1") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("e1") =>
                        castlingPositionABool = false
                      case _ => {}
                    }
                  }
                  castlingPositionABool
                } && {
                  _currentBoard("b1") == InfoNone && _currentBoard("c1") == InfoNone && _currentBoard(
                    "d1"
                  ) == InfoNone
                }) castlingPosition = ExLocation("c1") +: castlingPosition
          }
          val castlingPositionH = {
            var castlingPositionABool = true
            if (init == true &&
                (
                  _currentBoard("h1") match {
                    case InfoWhite(kind, init) =>
                      if (kind == Rook && init == true) true else false
                    case _ => false
                  }
                ) && {
                  for (i <- forKingRangeB) {
                    i._2 match {
                      case a if a contains ExLocation("g1") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("e1") =>
                        castlingPositionABool = false
                      case b if b contains ExLocation("f1") =>
                        castlingPositionABool = false
                      case _ => {}
                    }
                  }
                  castlingPositionABool
                } && {
                  _currentBoard("f1") == InfoNone && _currentBoard("g1") == InfoNone
                }) castlingPosition = ExLocation("g1") +: castlingPosition
          }
          //
          rangeFinderMMap(i._1) =
            (rangeFinderMMap(i._1) diff whenMoveCheck.distinct) ++ castlingPosition
        case _ => {}
      }
    }

    rangeFinderMMap
  }

}

object PieceRule {
  def apply(currentBoard: MMap[String, Info]) = {
    val inst = new PieceRule(currentBoard)
    inst.rangeFinder
  }

  def apply(currentBoard: MMap[String, Info], isWhite: Boolean) = {
    val inst = new PieceRule(currentBoard)
    val rangeOfAll: MMap[String, Array[ExLocation]] = inst.rangeFinder

    val bwKingLoc = Array("Null", "Null")
    for (i <- currentBoard) {
      i._2 match {
        case InfoBlack(kind, init) if kind == King => bwKingLoc(0) = i._1
        case InfoWhite(kind, init) if kind == King => bwKingLoc(1) = i._1
        case _                                     => {}
      }
    }

    val blacks = {
      var tmp = Array[String]()
      for (tup <- currentBoard) {
        tup._2 match {
          case InfoBlack(kind, init) => tmp = tmp appended tup._1
          case _                     => {}
        }
      }
      tmp
    }
    val whites = {
      var tmp = Array[String]()
      for (tup <- currentBoard) {
        tup._2 match {
          case InfoWhite(kind, init) => tmp = tmp appended tup._1
          case _                     => {}
        }
      }
      tmp
    }

    val findIfChecked: Boolean = {
      if (isWhite) {
        var tmp = false
        for (i <- rangeOfAll) {
          if (i._2 contains ExLocation(bwKingLoc(1))) tmp = true
        }
        tmp
      } else {
        var tmp = false
        for (i <- rangeOfAll) {
          if (i._2 contains ExLocation(bwKingLoc(0))) tmp = true
        }
        tmp
      }
    }

    if (isWhite) {
      if (findIfChecked) {
        MMap(bwKingLoc(1) -> rangeOfAll(bwKingLoc(1)))
      } else {
        rangeOfAll.filter(tup =>
          tup match {
            case (a, b) if whites contains a => true
            case _                           => false
          }
        )
      }
    } else {
      if (findIfChecked) {
        MMap(bwKingLoc(0) -> rangeOfAll(bwKingLoc(0)))
      } else {
        rangeOfAll.filter(tup =>
          tup match {
            case (a, b) if blacks contains a => true
            case _                           => false
          }
        )
      }
    }

  }

}
