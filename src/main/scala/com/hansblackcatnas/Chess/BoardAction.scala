package com.hansblackcat.Chess
import scala.collection.mutable.{Map=>MMap, ListBuffer}

class BoardAction extends Root with PGN {
    // TagPair
    // Comment
    // private[this] var commentBuffer: MMap[Int, Comment]

    private[this] var currentBoard: MMap[String, Info] = MMap.empty
    private[this] var historyBoard: ListBuffer[MMap[String,Info]] = ListBuffer()

    private[this] var currentPieceRule: MMap[String,Array[ExLocation]] = MMap.empty

    // (whiteAction, blackAction)
    private[this] var historyMoveText = ListBuffer[(String,String)]()

    // TODO make this to case class
    private[this] var moveTextBuffer = "" // also check white, black .isEmpty

    private[this] def _start(i: String) = {
        currentBoard = i match {
            case "base"  => baseMapHash
            case "test1" => testGrid1
            case "test2" => testGrid2
            case "test3" => testGrid3
            case _       => baseMapHash
        }
        currentPieceRule = PieceRule(currentBoard)
        historyMoveText = ListBuffer[(String,String)]()
        moveTextBuffer = ""
    }

    // TODO: add TagPair
    def start(i : String) = _start(i)
    def start() = _start("base")

    // TDDO `{}` index
    def commentHere(comment: String) = {}

    def currentBoardShow() = {
        currentBoard
    }
    def currentInfoShow(ipt: String): Info = {
        require(ipt.length() == 2)
        currentBoard(ipt)
    }
    def currentUniShow(ipt: String): String = {
        toUni(currentInfoShow(ipt))
    }

    def isExistHere(ipt: String): Boolean = {
        currentBoard(ipt) match {
            case InfoNone => false
            case _ => true
        }
    }

    def possibleShow(ipt: String) = {
        require(ipt.length() == 2)
        _debugPrintR(currentPieceRule(ipt))
    }

    def possibleShowAll() = {
        PieceRule(currentBoard)
    }

    def allHistoryMTShow() = {
        println("< Right Before >")
        println(moveTextBuffer)
        println("< History >")
        println(historyMoveText)
    }

    def actWithMoveTest(ipt: String) = {
        // act sth
        moveTextBuffer match {
            case a if a.isEmpty  => moveTextBuffer = ipt
            case b if !b.isEmpty => 
                historyMoveText = historyMoveText :+ (b, ipt)
                moveTextBuffer = ""
        }

    }





    def toUni(ipt: Info) = {
            ipt match {
                case InfoNone => ""
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


    
    // PGN Interpreter

    // 1. Bb5 cxb5 Bxf7+ O-O e8=Q# 
    def setMoveText(ipt: String) = {
        var txt = ipt
        var isTurn = false
        var whats = Array[PGNSpecial]()
        var whos: Option[PGNPieceKind] = None // Some[T](cont) or None
        var wheres: Option[ExLocation] = None 

        if ('1' to '9' contains txt.head) {
            isTurn = true
        } else {
            for (i <- 1 to 4) {
                txt match {
                    case cap if txt.contains('x') =>
                        val idx = txt.indexOf('x')
                        whats = Capture +: whats
                        txt = txt.substring(0,idx) ++ txt.substring(idx+1)
                    case cm if txt.contains('#') =>
                        whats = CheckMateingMove +: whats
                        txt = txt.substring(0, txt.length - 1)
                        // End - Game
                    case ck if txt.contains('+') =>
                        whats = CheckingMove +: whats
                        txt = txt.substring(0, txt.length - 1)
                    case pro if txt.contains('=') =>
                        val idx = txt.indexOf('=')
                        val promotionTo = txt(idx+1)
                        promotionTo match {
                            case 'Q' => whats = Promotion(Queen) +: whats
                            case 'R' => whats = Promotion(Rook) +: whats
                            case 'B' => whats = Promotion(Bishop) +: whats
                            case 'N' => whats = Promotion(Knight) +: whats
                        }
                        txt = txt.substring(0,idx)
                    case _ => {}
                }
            }

            if (txt.head == 'O') {
                txt match {
                    case "O-O-O" => whats = QSCastling +: whats
                    case "O-O" => whats = KSCastling +: whats
                    case _ => throw new Exception("Wrong Castling Pattern input") 
                }
            } else if (txt.head == txt.head.toUpper) {
                txt.head match {
                    case 'K' => whos = Some(King)
                    case 'Q' => whos = Some(Queen)
                    case 'R' => whos = Some(Rook)
                    case 'B' => whos = Some(Bishop)
                    case 'N' => whos = Some(Knight)
                }

                txt.tail match {
                    case a if a.length == 3 =>
                        whats = OriginFromRow(a.head) +: whats
                        wheres = Some(a.tail(0) #> a.tail(1))
                    case b if b.length == 2 =>
                        wheres = Some(b(0) #> b(1)) 
                }
            } else {
                whos = Some(Pawn)
                txt match {
                    case a if a.length == 3 =>
                        whats = OriginFromRow(a.head) +: whats
                        wheres = Some(a.tail(0) #> a.tail(1))
                    case b if b.length == 2 =>
                        wheres = Some(b(0) #> b(1))
                }
            }
        }

        CurrentMoveText(ipt, isTurn, whats, whos, wheres)
    }


    def checkAvail() = {}
    def action() = {

    }

    // For debug printing
    def debugPrintBoard = {
        _debugPrintB(currentBoard)
    }
    def debugPrintRangeAll = {
        for (i <- currentBoard) {
            i._2 match {
                case InfoNone => {}
                case InfoWhite(kind, _) => 
                    println("<< DEBUG Print Range >>")
                    println(s"-- ${kind} in ${i._1} --")
                    _debugPrintR(PieceRule(currentBoard)(i._1))
                case InfoBlack(kind, _) => 
                    println("<< DEBUG Print Range >>")
                    println(s"-- ${kind} in ${i._1} --")
                    _debugPrintR(PieceRule(currentBoard)(i._1))
            }
        }
    }
    
}