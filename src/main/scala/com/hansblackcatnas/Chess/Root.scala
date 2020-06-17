package com.hansblackcat.Chess
import scala.collection.mutable.{Map=>MMap}
import scala.math._

class Root {
    implicit class ArrayToExLocation (arr: Array[Int]) {
        def toExLoc() = {
            require(
                arr.length == 2 &&
                0 <= arr(0) && arr(0) <8 &&
                0 <= arr(1) && arr(1) <8
            )
            ExLocation((arr(0)+97).toChar +: (arr(1)+1).toString())
        }
    }
    implicit class ArrayLikeVector (arr: Array[Int]) {
        def +> (tup: (Int, Int)) = {
            Array(arr(0)+tup._1, arr(1)+tup._2)
        }
        def +> (arr2: Array[Int]) = {
            require(arr.length == 2)
            Array(arr(0)+arr2(0), arr(1)+arr2(1))
        }
        def -> (tup: (Int, Int)) = {
            Array(arr(0)-tup._1, arr(1)-tup._2)
        }
        def -> (arr2: Array[Int]) = {
            require(arr.length == 2)
            Array(arr(0)-arr2(0), arr(1)-arr2(1))
        }
        def toUnit() = {
            def safeUnit(a: Int) = if (a != 0) { a / abs(a) } else a
            Array(safeUnit(arr(0)), safeUnit(arr(1)))
        }
    }

    implicit class ExLocationMaker (row: Char) {
        /*
        def #> (colum: Int) = {
            ExLocation(row +: colum.toString)
        }
        */
        def #> (colum: Char) = {
            ExLocation(row +: colum.toString)
        }
    }
    implicit class ExLocationMaker2 (row: Int) {
        def #> (colum: Int) = {
            val tmp = (row + 96).toChar
            ExLocation(tmp +: colum.toString)
        }
    }
    implicit class InfoColorMaker (kind: PGNPieceKind) {
        def <<@> () = {
            InfoWhite(kind, true)
        }
        def <<#> () = {
            InfoWhite(kind, false)
        }
        def <@>> () = {
            InfoBlack(kind, true)
        }
        def <#>> () = {
            InfoBlack(kind, false)
        }
    }
    val infoNone: Info = InfoNone
    def infoBlackWhite(info: Info) = {
        info match {
            case InfoNone        => throw new Exception("call of None type")
            case InfoBlack(_, _) => false
            case InfoWhite(_, _) => true
        }
    }

    // Vector(a1, b1, c1, ..., a2, b2, ...
    val baseGridKeys = for (j <- 1 to 8; i <- 'a' to 'h') yield i +: j.toString()

    val baseGridValue: Vector[Info] = Vector(
        Rook.<<@>, Knight.<<@>, Bishop.<<@>, Queen.<<@>, King.<<@>, Bishop.<<@>, Knight.<<@>, Rook.<<@>,
        Pawn.<<@>, Pawn.<<@>, Pawn.<<@>, Pawn.<<@>, Pawn.<<@>, Pawn.<<@>, Pawn.<<@>, Pawn.<<@>,
    ) ++
        ((for (i <- 1 to 32) yield infoNone).toVector) ++
        Vector(
        Pawn.<@>>, Pawn.<@>>, Pawn.<@>>, Pawn.<@>>, Pawn.<@>>, Pawn.<@>>, Pawn.<@>>, Pawn.<@>>,
        Rook.<@>>, Knight.<@>>, Bishop.<@>>, Queen.<@>>, King.<@>>, Bishop.<@>>, Knight.<@>>, Rook.<@>>,
    )
    val baseMapHashTMP = baseGridKeys.zip(baseGridValue).toMap
    val baseMapHash = MMap(baseMapHashTMP.toSeq: _*)
    
    val _emptyGrid = (baseGridKeys zip (for (i <- 1 to 64) yield infoNone)).toMap
    val emptyGrid: MMap[String, Info] = MMap(_emptyGrid.toSeq: _*)

    val testGrid1 = {
        var tmpGrid = emptyGrid.clone()
        tmpGrid("f3") = Queen.<#>>; tmpGrid("e1") = King.<<@>; tmpGrid("d3") = Rook.<#>>; tmpGrid("e5") = Knight.<<#>
        tmpGrid("g4") = Bishop.<<#>; tmpGrid("a5") = Pawn.<@>>; tmpGrid("b5") = Pawn.<<#>; tmpGrid("h1") = Rook.<<@>
        tmpGrid("c2") = Pawn.<<@>; tmpGrid("d8") = King.<@>>; tmpGrid("d7") = Bishop.<<#>; tmpGrid("d6") = Rook.<<#>
        tmpGrid("g8") = Knight.<<#>
        tmpGrid
    }

    // For Castling Test
    val testGrid2 = {
        var tmpGrid = emptyGrid.clone()
        tmpGrid("e1") = King.<<@>; tmpGrid("e8") = King.<@>>; tmpGrid("f4") = Rook.<<#> 
        tmpGrid("h1") = Rook.<<@>; tmpGrid("h8") = Rook.<@>>; tmpGrid("a1") = Rook.<<@>; tmpGrid("a8") = Rook.<@>>
        tmpGrid
    }
    val testGrid3 = {
        var tmpGrid = emptyGrid.clone()
        tmpGrid("e1") = King.<<@>; tmpGrid("e8") = King.<@>>; 
        tmpGrid("h1") = Rook.<<@>; tmpGrid("h8") = Rook.<@>>; tmpGrid("a1") = Rook.<<@>; tmpGrid("a8") = Rook.<@>>
        tmpGrid("g4") = Rook.<<#>; tmpGrid("c3") = Bishop.<#>>
        tmpGrid
    }
    val testGrid4 = {
        var tmpGrid = emptyGrid.clone()
        tmpGrid("e1") = King.<<@>; tmpGrid("e8") = King.<@>>; tmpGrid("f4") = Rook.<<#> 
        tmpGrid("h1") = Rook.<<@>; tmpGrid("h8") = Rook.<@>>; tmpGrid("a1") = Rook.<<@>; tmpGrid("a8") = Rook.<@>>
        tmpGrid("e7") = Rook.<#>>
        tmpGrid
    }
    val testGrid5 = {
        var tmpGrid = emptyGrid.clone()
        tmpGrid("f8") = King.<#>>; tmpGrid("f5") = King.<<#>; tmpGrid("f7") = Pawn.<<#>; tmpGrid("b2") = Bishop.<#>>
        tmpGrid
    }

    def infoCloneWithNotInit(i: Info): Info = {
        i match {
            case InfoNone => { throw new Exception("Trying to clean empty Info") }
            case InfoBlack(kind, init) => 
                InfoBlack(kind, false)
            case InfoWhite(kind, init) => 
                InfoWhite(kind, false)
        }
    }

    def _debugPrintB(currentBoard: MMap[String, Info]) = {
        var str: String = ""
        def toUni(ipt: Info) = {
            ipt match {
                case InfoNone => "\u2022"
                case InfoWhite(kind, _) => 
                    kind match {
                        case King => "\u2654"
                        case Queen => "\u2655"
                        case Rook => "\u2656"
                        case Bishop => "\u2657"
                        case Knight => "\u2658"
                        case Pawn => "\u2659"
                    }
                case InfoBlack(kind, _) => 
                    kind match {
                        case King => "\u265A"
                        case Queen => "\u265B"
                        case Rook => "\u265C"
                        case Bishop => "\u265D"
                        case Knight => "\u265E"
                        case Pawn => "\u265F"
                    }
            }
        }

        for (j <- (1 to 8).reverseIterator; i <- 'a' to 'h') {
            val key = s"$i$j"
            str = str ++ toUni(currentBoard(key)) ++ " "
            if (i == 'h') str = str ++ "\n"
        }
        str
    }

    def _debugPrintR(arr: Array[ExLocation]) = {
        for (j <- (1 to 8).reverseIterator; i <- 'a' to 'h') {
            if (arr.contains(ExLocation(s"$i$j"))) print("\u265F ")
            else print("\u2659 ")
            if (i == 'h') println("")
        }
    }
}