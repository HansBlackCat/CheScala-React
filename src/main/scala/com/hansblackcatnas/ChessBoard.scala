package com.hansblackcatnas

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.timers._

import org.scalajs.dom
import slinky.core.facade.ReactElement
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.css
import com.hansblackcat.Chess._
import scala.collection.mutable.{Map => MMap, ListBuffer}


@JSImport("resources/ChessBoard.css", JSImport.Default) 
@js.native
object ChessBoardCSS extends js.Object

@JSImport("resources/ChessFront.css", JSImport.Default)
@js.native
object ChessFrontCSS extends js.Object

@JSImport("resources/logochescala.svg", JSImport.Default)
@js.native
object CheScalaLogo extends js.Object

object ChessMain {
    var newBoard = new BoardAction
    newBoard.start("test5")
    var isWhite = newBoard.boardisWhite

    // Board copy, isWhite
    var historyBoard: ListBuffer[(BoardAction, Boolean)] = ListBuffer((newBoard.deepClone, true))
    // (from, to, isWhite)
    var simpleHistory = ListBuffer[(String, String, PGNPieceKind, Boolean)](("Null", "Null", Pawn, true))
    // For switching
    var tempBoard: ListBuffer[(BoardAction, Boolean)] = ListBuffer()
    
    private[this] val numLocKeyPair = {
        val value = (for (i <- (1 to 8).reverseIterator; j <- ('a' to 'h')) yield (s"$j" + s"$i")).toArray
        val key = (for (i <- 0 to 63) yield i).toArray
        key.zip(value).toMap -> value.zip(key).toMap
    }

    def purge() = {
        newBoard.start()
        isWhite = true
        historyBoard = ListBuffer()
        simpleHistory = ListBuffer()
        tempBoard = ListBuffer()
    }
    def backtoPast(i: Int) = {
        newBoard = historyBoard(i)._1
        isWhite = newBoard.boardisWhite
        historyBoard = historyBoard.slice(i+1, historyBoard.length)
        simpleHistory = simpleHistory.slice(i+1, simpleHistory.length)
    }

    def currentBoardShow() = newBoard.currentBoardShow()
    def currentBoardDebugShow() = newBoard.debugPrintBoard

    def isExistHere(ipt: String) = newBoard.isExistHere(ipt)
    def isExistHere(ipt2: Int) = newBoard.isExistHere(numLocKeyPair._1(ipt2))

    def currentUniShow(str: String) = newBoard.currentUniShow(str)
    def currentRangeMMap_Raw = PieceRule(currentBoardShow())
    def currentRangeMMap(str: String, isWhite: Boolean) = PieceRule(currentBoardShow(), isWhite)(str).map(i => numLocKeyPair._2(i.location))
    def currentRangeMMap_Sup(str: String, isWhite: Boolean) = PieceRule(currentBoardShow(), isWhite).keys.toIndexedSeq

    def act(from: String, to: String) = newBoard.act(from, to, ChessMain.isWhite)
    

    def curkingLoc(isWhite: Boolean) = newBoard.kingLocation(isWhite)
    def promotion(loc: String, toward: String) = newBoard.actAndSub(loc, !ChessMain.isWhite, toward)

    def ischeck() = {
        var booltmp = false
        for (i <- PieceRule(currentBoardShow(), !isWhite)) {
            if (i._2 contains ExLocation(curkingLoc(isWhite))) booltmp = true
        }

        def fiftyPawnCheck(ipt: ListBuffer[(String, String, PGNPieceKind, Boolean)]) = {
            val tmp = ipt.slice(0,50)
            var tmpBool = false
            for (i <- tmp) {
                if (i._3 == Pawn) tmpBool = true
            } 
            tmpBool
        } 

        if (
            simpleHistory.length > 10 &&
            simpleHistory(0) == simpleHistory(4) && simpleHistory(4) == simpleHistory(8) &&
            simpleHistory(1) == simpleHistory(5) && simpleHistory(5) == simpleHistory(9)
        ) "ThreeFold"
        else if (
            simpleHistory.length > 50 &&
            fiftyPawnCheck(simpleHistory) &&
            PieceRule(historyBoard(50)._1.currentBoardShow()).keys.size == PieceRule(currentBoardShow()).keys.size
        ) "Fifty-move"
        else if (booltmp && PieceRule(currentBoardShow(), isWhite)(ChessMain.curkingLoc(isWhite)).isEmpty) "CheckMate"
        else if (booltmp) "Check"
        else if (!booltmp) {
            var tmp = 0
            for (l <- PieceRule(currentBoardShow(), isWhite)) {
                tmp = tmp + l._2.length
            }
            if (tmp == 0) "StaleMate"
            else ""
        }
        else ""


    }


    // TODO: Check, CheckMate, Stale, Draw, Promotion, Pawn Attack Range Management
}

@react object ChessSquare {
  case class Props(
    value: String,
    bgColor: String,
    bgBool: Boolean,
    onClick: () => Unit,
    onMouseOver: () => Unit,
    onMouseLeave: () => Unit,
    actionSurge: () => Unit
  )


  val component = FunctionalComponent[Props] { props =>
    button(
        className:="square", 
        onClick:={props.onClick},
        onMouseOver:={props.onMouseOver},
        onMouseLeave:={props.onMouseLeave}, 
        style:=js.Dynamic.literal(
            backgroundColor = s"${props.bgColor}"
    ))(
      props.value
    )
  }
}

@react object EmptySquare {
    case class Props(
        value: String
    )

    val component = FunctionalComponent[Props] { props =>
        button(
            className:="emptySquare"
        )(
            props.value
        )
    }
}


@react class ChessBoard extends Component {
    type Props = Unit
    case class State(
        square: Array[Int],
        // TODO: Add Array Special Action to additional action
        board: Array[String],
        histroyChecker: Array[Int]
    )

    def initialState: State = {
        val tmp = Array.fill(64)(0)
        for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0
        State(tmp, objectBoardPuller(), Array())
    }

    private val css = ChessBoardCSS

    def intToChessLoc(loc: Int) = {
        val tmp = (for (i <- (1 to 8).reverseIterator; j <- ('a' to 'h')) yield (s"$j" + s"$i")).toArray
        tmp(loc)
    }

    
    def renderSquare(i: Int): ReactElement = {
        ChessSquare(
            // TODO: MORE Flexible
            value = this.state.board(i),
            bgColor = {
                // OnBase -- 0 -> Non-Exist, 1 -> Exist
                if (((i/8)%2+i)%2 == 0 && 0 <= this.state.square(i) && this.state.square(i) < 2) "#ffcf9f" 
                // OnHover -- rangeFinder
                else if (this.state.square(i) == 2) "#FC4445"
                // OnClick -- Chooser
                else if (this.state.square(i) == 3) "#97CAEF"
                // OnBase2
                else "#d28c45"
            },
            bgBool = false,
            onClick = {() => this.handleClick(i)},
            onMouseLeave = {() => this.handleLeave(i)},
            onMouseOver = {() => this.handleOver(i)},
            actionSurge = {() => this.actionSurge(i)}
        )
    }

    def renderEmptySquare(str: String): ReactElement = {
        EmptySquare(
            value = str
        )
    }

    def objectBoardPuller() = {
        val tmp = Array.fill(64)("")
        for (i <- 0 to 63) {
            tmp(i) = ChessMain.currentUniShow(intToChessLoc(i))
        }
        tmp
    }

    def historyCheckerPuller() = {
        Array.fill(ChessMain.historyBoard.length)(1)
    }


    def handleLeave(i: Int) = {
        //this.setState(initialState)
    }
    def handleOver(i: Int) = {
        var squareState = this.state.square.clone()

        if (squareState.contains(2) && !squareState.contains(3)) squareState = initialState.square

        if ((ChessMain.currentRangeMMap_Sup(intToChessLoc(i), ChessMain.isWhite) contains intToChessLoc(i)) 
        && !squareState.contains(3) 
        ) {
            for (k <- ChessMain.currentRangeMMap(intToChessLoc(i), ChessMain.isWhite)) {
                squareState(k) = 2
            }
        }
        this.setState(State(squareState, objectBoardPuller(), state.histroyChecker))
    }

    def handleClick(i: Int) = {
        val squareState = this.state.square.clone()
        val stateNew: State = squareState(i) match {
            case 0 =>
                val tmp = Array.fill(64)(0)
                for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0
                State(tmp, objectBoardPuller(), state.histroyChecker)
            case 1 => 
                val tmp = squareState
                if (!squareState.contains(3)) tmp(i) = 3
                State(tmp, this.state.board, state.histroyChecker)
            case 2 => 
                if (squareState contains 3) {
                    val from = squareState indexOf 3
                    val to = i


                    actWrap(from, to)
                } else initialState
            case 3 =>
                val tmp = squareState
                tmp(i) = 1
                State(tmp, this.state.board, state.histroyChecker)
        }
        this.setState(stateNew)
    }

    def actionSurge(i: Int) = {

    }

    def actWrap(from: Int, to: Int) = {
        val fromLoc = intToChessLoc(from) 
        val toLoc = intToChessLoc(to)
        
        val kind = ChessMain.currentBoardShow()(fromLoc) match {
            case InfoWhite(kind, init) => kind
            case InfoBlack(kind, init) => kind
        }

        // History Save
        val forStack = ChessMain.newBoard

        ChessMain.simpleHistory.prepend((fromLoc, toLoc, kind, ChessMain.isWhite))
        ChessMain.historyBoard.prepend((ChessMain.newBoard.deepClone, ChessMain.isWhite))

        ChessMain.act(fromLoc, toLoc)
        ChessMain.isWhite = !ChessMain.isWhite



        ChessMain.tempBoard.prepend((ChessMain.newBoard, ChessMain.isWhite))
        val tmp = Array.fill(64)(0)
        for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0

        // Promotion! Lovely
        // TODO
        val whitePromotionBlocks = (for (i <- 'a' to 'h') yield i +: "8").toArray
        val blackPromotionBlocks = (for (i <- 'a' to 'h') yield i +: "1").toArray
        for (wP <- whitePromotionBlocks) {
            if (ChessMain.newBoard.currentUniShow(wP) == "\u2659") {
                ChessMain.promotion(wP, "a")
            }
        }
        for (bP <- blackPromotionBlocks) {
            if (ChessMain.newBoard.currentUniShow(bP) == "\u265F") {
                ChessMain.promotion(bP, "a")
            }
        }


        // Check Match
        ChessMain.ischeck() match {
            case "Check" => dom.window.alert("Check")
            case "CheckMate" => dom.window.alert("CheckMate"); ChessMain.purge()
            case "StaleMate" => dom.window.alert("StaleMate"); ChessMain.purge()
            case "ThreeFold" => dom.window.alert("ThreeFold"); ChessMain.purge()
            case "Fifty-move" => dom.window.alert("Fifty-move"); ChessMain.purge()
            case _ => {}
        }

        State(tmp, objectBoardPuller(), historyCheckerPuller())
    }

    
    def callHistroy(i: Int) = {
        ChessMain.backtoPast(i)

        val objectBP = objectBoardPuller()
        
        val tmp = Array.fill(64)(0)
        for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0
        this.setState(State(tmp, objectBP, historyCheckerPuller()))
    }


    def render(): ReactElement = {
        val historyChecker = state.histroyChecker
        val history = ChessMain.tempBoard
        val tmpHis = ChessMain.tempBoard
        val moves = {
            var tempSeq = IndexedSeq[ReactElement]()
            for (j <- 0 until historyChecker.length) {
                val liBuild = li(className:="ListHistory")(
                    button(className:="ButtonCallHistory", onClick:={() => callHistroy(j)})(
                        s"Turn ${historyChecker.length - j -1}",
                        b(className:="ButtonCallHistory_InnerTags")(
                            "\tYo"
                        )
                    ),
                )
                tempSeq = liBuild +: tempSeq
            }
            tempSeq
        }

        div(className:="ChessBoard-Total-Render")(
            div(className := "LeftBoard")(
                header(
                    h5(className := "Head")("Welcome to CheScala")
                ),
                p(
                    button(className:="Temp-Clear-Button", onClick:={() => ChessMain.purge;this.setState(initialState);})(
                        "Restart"
                    )
                ),
                p(
                    button(onClick:={() => ChessMain.currentBoardDebugShow()})(
                        "Debug Print"
                    )
                ),
                p(className:="LeftHistory")(
                    div(className:="LeftHistory-Head")("History"),
                    ol(moves)
                )
            ),
            div(className:="Total-Board")(
                div(className:="Board-Row")(
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("8"),
                    renderSquare(0),
                    renderSquare(1),
                    renderSquare(2),
                    renderSquare(3),
                    renderSquare(4),
                    renderSquare(5),
                    renderSquare(6),
                    renderSquare(7),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("7"),
                    renderSquare(8),
                    renderSquare(9),
                    renderSquare(10),
                    renderSquare(11),
                    renderSquare(12),
                    renderSquare(13),
                    renderSquare(14),
                    renderSquare(15),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("6"),
                    renderSquare(16),
                    renderSquare(17),
                    renderSquare(18),
                    renderSquare(19),
                    renderSquare(20),
                    renderSquare(21),
                    renderSquare(22),
                    renderSquare(23),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("5"),
                    renderSquare(24),
                    renderSquare(25),
                    renderSquare(26),
                    renderSquare(27),
                    renderSquare(28),
                    renderSquare(29),
                    renderSquare(30),
                    renderSquare(31),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("4"),
                    renderSquare(32),
                    renderSquare(33),
                    renderSquare(34),
                    renderSquare(35),
                    renderSquare(36),
                    renderSquare(37),
                    renderSquare(38),
                    renderSquare(39),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("3"),
                    renderSquare(40),
                    renderSquare(41),
                    renderSquare(42),
                    renderSquare(43),
                    renderSquare(44),
                    renderSquare(45),
                    renderSquare(46),
                    renderSquare(47),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("2"),
                    renderSquare(48),
                    renderSquare(49),
                    renderSquare(50),
                    renderSquare(51),
                    renderSquare(52),
                    renderSquare(53),
                    renderSquare(54),
                    renderSquare(55),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare("1"),
                    renderSquare(56),
                    renderSquare(57),
                    renderSquare(58),
                    renderSquare(59),
                    renderSquare(60),
                    renderSquare(61),
                    renderSquare(62),
                    renderSquare(63),
                    renderEmptySquare(""),
                ),
                div(className:="Board-Row")(
                    renderEmptySquare(""),
                    renderEmptySquare("a"),
                    renderEmptySquare("b"),
                    renderEmptySquare("c"),
                    renderEmptySquare("d"),
                    renderEmptySquare("e"),
                    renderEmptySquare("f"),
                    renderEmptySquare("g"),
                    renderEmptySquare("h"),
                    renderEmptySquare(""),
                ),
            )
        )
    }
}

@react class Front extends Component {
    type Props = Unit
    case class State(
        a: Boolean
    )

    def initialState: State = State(true)

    private val css = ChessFrontCSS

    def render() = {
        div(className:="Front")(
            header(className:="FrontHeader")(
                //img(src := CheScalaLogo.asInstanceOf[String], alt := "logo-chescala"),
                h1(className:="HeaderTitle")(
                    "Welcome to CheScala"
                ),
                //h2(CheScalaLogo.asInstanceOf[String])
            ),
            p(className:="ChessBoard")(
                ChessBoard()
            )
        )
    }
}




























// Example + Test
@react class StateExample extends Component {
    type Props = Unit
    case class State(
        loading: Boolean,
        formData: String
    )

    //private val css = ChessBoardCSS

    def initialState: State = {
        State(loading = true, formData = "No Data")
    }


    override def componentDidMount() = {
        //After DOM rendered, before view do sth//
    }

    // get DOM inform ex) where is scroll?, what is rendered elements size?
    // override def getSnapshotBeforeUpdate(prevProp, prevState)


    def handleData() {
        val rep_data = "New Data"
        this.setState(State(loading = false, formData = rep_data))
    }

    def buttonData() = {
        val rep_data = "Button Clicked"
        this.setState(State(loading = false, formData = rep_data))
    }


    def render(): ReactElement = {

        return (
            div(
                span(
                    s"Loading Process...: ${this.state.loading}"
                ),
                span(
                    s"Here!          ...: ${this.state.formData}"
                ),
                p(
                    button(className := "Button", onMouseOver:=(_ => buttonData()), onMouseLeave:=(_ => handleData()))(
                        "Hello"
                    )
                )
            ),
        
        )
    }
}


