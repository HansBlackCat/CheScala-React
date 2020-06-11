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

object ChessMain {
    val newBoard = new BoardAction
    newBoard.start()
    var isWhite = newBoard.boardisWhite

    var historyBoard: ListBuffer[(BoardAction, Boolean)] = ListBuffer()
    var simpleHistory = ListBuffer(1)
    // For switching
    var tempBoard: ListBuffer[(BoardAction, Boolean)] = ListBuffer()
    
    private[this] val numLocKeyPair = {
        val value = (for (i <- (1 to 8).reverseIterator; j <- ('a' to 'h')) yield (s"$j" + s"$i")).toArray
        val key = (for (i <- 0 to 63) yield i).toArray
        key.zip(value).toMap -> value.zip(key).toMap
    }


    def currentBoardShow() = newBoard.currentBoardShow()
    def currentBoardDebugShow() = newBoard.debugPrintBoard

    def isExistHere(ipt: String) = newBoard.isExistHere(ipt)
    def isExistHere(ipt2: Int) = newBoard.isExistHere(numLocKeyPair._1(ipt2))

    def currentUniShow(str: String) = newBoard.currentUniShow(str)
    def currentRangeMMap(str: String, isWhite: Boolean) = PieceRule(currentBoardShow(), isWhite)(str).map(i => numLocKeyPair._2(i.location))
    def currentRangeMMap_Sup(str: String, isWhite: Boolean) = PieceRule(currentBoardShow(), isWhite).keys.toIndexedSeq

    def act(from: String, to: String) = newBoard.act(from, to, ChessMain.isWhite)

    // TODO: Check, CheckMate, Steil, Draw, Promotion
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



@react class ChessBoard extends Component {
    type Props = Unit
    case class State(
        square: Array[Int],
        // TODO: Add Array Special Action to additional action
        board: Array[String]
    )

    def initialState: State = {
        val tmp = Array.fill(64)(0)
        for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0
        State(tmp, objectBoardPuller())
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
                else if (this.state.square(i) == 2) "red"
                // OnClick -- Chooser
                else if (this.state.square(i) == 3) "blue"
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

    def objectBoardPuller() = {
        val tmp = Array.fill(64)("")
        for (i <- 0 to 63) {
            tmp(i) = ChessMain.currentUniShow(intToChessLoc(i))
        }
        tmp
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
        this.setState(State(squareState, objectBoardPuller()))
    }

    def handleClick(i: Int) = {
        val squareState = this.state.square.clone()
        val stateNew: State = squareState(i) match {
            case 0 => initialState
            case 1 => 
                val tmp = squareState
                if (!squareState.contains(3)) tmp(i) = 3
                State(tmp, this.state.board)
            case 2 => 
                if (squareState contains 3) {
                    val from = squareState indexOf 3
                    val to = i
                    actWrap(from, to)
                } else initialState
            case 3 =>
                val tmp = squareState
                tmp(i) = 1
                State(tmp, this.state.board)
        }
        this.setState(stateNew)
    }

    def actionSurge(i: Int) = {

    }

    def actWrap(from: Int, to: Int) = {
        val fromLoc = intToChessLoc(from) 
        val toLoc = intToChessLoc(to)
        ChessMain.act(fromLoc, toLoc)
        ChessMain.isWhite = !ChessMain.isWhite
        // History Save

        val tmp = Array.fill(64)(0)
        for (k <- 0 to 63) if (ChessMain.isExistHere(k)) tmp(k) = 1 else 0

        State(tmp, objectBoardPuller())
    }

    def render(): ReactElement = {
        div(className:="ChessBoard-Total-Render")(
            header(
                h5(className := "Head")("Welcome to CheScala")
            ),
            p(
                "History"
            ),
            p(
                button(className:="Temp-Clear-Button", onClick:={() => this.setState(initialState);})(
                    "Restart"
                )
            ),
            p(
                button(onClick:={() => ChessMain.currentBoardDebugShow()})(
                    "Debug Print"
                )
            ),
            p(
                div(className:="Total-Board")(
                    div(className:="Board-Row")(
                        renderSquare(0),
                        renderSquare(1),
                        renderSquare(2),
                        renderSquare(3),
                        renderSquare(4),
                        renderSquare(5),
                        renderSquare(6),
                        renderSquare(7),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(8),
                        renderSquare(9),
                        renderSquare(10),
                        renderSquare(11),
                        renderSquare(12),
                        renderSquare(13),
                        renderSquare(14),
                        renderSquare(15),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(16),
                        renderSquare(17),
                        renderSquare(18),
                        renderSquare(19),
                        renderSquare(20),
                        renderSquare(21),
                        renderSquare(22),
                        renderSquare(23),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(24),
                        renderSquare(25),
                        renderSquare(26),
                        renderSquare(27),
                        renderSquare(28),
                        renderSquare(29),
                        renderSquare(30),
                        renderSquare(31),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(32),
                        renderSquare(33),
                        renderSquare(34),
                        renderSquare(35),
                        renderSquare(36),
                        renderSquare(37),
                        renderSquare(38),
                        renderSquare(39),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(40),
                        renderSquare(41),
                        renderSquare(42),
                        renderSquare(43),
                        renderSquare(44),
                        renderSquare(45),
                        renderSquare(46),
                        renderSquare(47),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(48),
                        renderSquare(49),
                        renderSquare(50),
                        renderSquare(51),
                        renderSquare(52),
                        renderSquare(53),
                        renderSquare(54),
                        renderSquare(55),
                    ),
                    div(className:="Board-Row")(
                        renderSquare(56),
                        renderSquare(57),
                        renderSquare(58),
                        renderSquare(59),
                        renderSquare(60),
                        renderSquare(61),
                        renderSquare(62),
                        renderSquare(63),
                    ),
                )
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

