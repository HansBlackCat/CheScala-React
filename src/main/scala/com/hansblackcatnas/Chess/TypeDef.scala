package com.hansblackcat.Chess

trait Location
case object NoneLocation extends Location
case class ExLocation(location: String) extends Location {
    val a = location.toCharArray().map(_.toInt)
    val toArrLoc = Array(a(0) - 97, a(1) - 49)
    def toStrLoc(toArrLoc: Array[Int]): String = (toArrLoc(0)+97).toChar.toString ++ (toArrLoc(1)+1).toString
    require(a.length < 3 && 0 <= toArrLoc(0) && toArrLoc(0) < 8 && 0 <= toArrLoc(1) && toArrLoc(1) < 8)

    def + (ipt: Array[Int]): ExLocation = {
        require(ipt.length < 3)
        val thisArr = this.toArrLoc
        ExLocation(toStrLoc(Array(thisArr(0)+ipt(0), thisArr(1)+ipt(1))))
    }
    def + (ipt: (Int, Int)): ExLocation = {
        val tmp = Array(ipt._1, ipt._2)
        this + tmp
    } 
    def - (ipt: Array[Int]): ExLocation = {
        this.+(ipt.map(_*(-1)))
    }
    def - (ipt: (Int, Int)): ExLocation = {
        val tmp = Array(ipt._1, ipt._2)
        this - tmp
    } 
    def R (row: Int): ExLocation = {
        this + Array(row, 0)
    }
    def C (colum: Int): ExLocation = {
        this + Array(0, colum)
    }
}


trait PGNComment
case class Comment(contents: String) extends PGNComment

trait PGNTagPairs
case class TPEvent(contents: String) extends PGNTagPairs
case class TPSite(contents: String) extends PGNTagPairs
case class TPDate(contents: String) extends PGNTagPairs
case class TPRound(contents: String) extends PGNTagPairs
case class TPWhite(contents: String) extends PGNTagPairs
case class TPNBlack(contents: String) extends PGNTagPairs
case class TPResult(contents: String) extends PGNTagPairs

trait PGNPieceKind
case object NoneKind extends PGNPieceKind
case object Pawn extends PGNPieceKind
case object King extends PGNPieceKind
case object Queen extends PGNPieceKind
case object Knight extends PGNPieceKind
case object Bishop extends PGNPieceKind
case object Rook extends PGNPieceKind

trait PGNSpecial
case object PGNNone extends PGNSpecial
case class OriginFromRow(char: Char) extends PGNSpecial
case object Capture extends PGNSpecial
case object QSCastling extends PGNSpecial
case object KSCastling extends PGNSpecial
case class Promotion(kind: PGNPieceKind) extends PGNSpecial
case object CheckingMove extends PGNSpecial
case object CheckMateingMove extends PGNSpecial

trait PGNMoveText extends PGNPieceKind with PGNSpecial with Location
case class MoveRound(round: Int) extends PGNMoveText
case class White(who: PGNPieceKind, where: Location, what: PGNSpecial*) extends PGNMoveText
case class Black(who: PGNPieceKind, where: Location, what: PGNSpecial*) extends PGNMoveText

trait PGN extends PGNTagPairs with PGNMoveText with PGNComment



trait Info
case object InfoNone extends Info
case class InfoWhite(kind: PGNPieceKind, init: Boolean) extends Info
case class InfoBlack(kind: PGNPieceKind, init: Boolean) extends Info


case class CurrentMoveText(
    ipt: String, isTurn: Boolean, whats: Array[PGNSpecial], whos: Option[PGNPieceKind], wheres: Option[ExLocation]
)
