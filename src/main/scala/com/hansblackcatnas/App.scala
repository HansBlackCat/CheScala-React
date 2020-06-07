package com.hansblackcatnas

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom
import slinky.core.facade.ReactElement

import com.hansblackcatnas.{MainPage, CComponent, ChessBoard}

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object


@react object Square {
  case class Props(
    value: String,
    onClick: () => Unit
  )

  val component = FunctionalComponent[Props] { props =>
    button(className:="square", onClick:={props.onClick})(
      props.value
    )
  }
}

@react class Board extends Component {
  type Props = Unit
  case class State(
    square: Array[Option[String]],
    xIsNext: Boolean
  )

  def initialState: State = State(Array.fill(9)(None), true)

  def handleClick(i: Int) = {
    val squares = state.square.clone()
    squares(i) = if (this.state.xIsNext) Option("X") else Option("O")
    this.setState(_ => State(
      squares,
      !this.state.xIsNext
    ))
  }

  def renderSquare(i: Int): ReactElement = {
    Square(
      value = this.state.square(i).getOrElse(""),
      onClick = {() => this.handleClick(i)}
    )
  }

  def render(): ReactElement = {
    val status = "Next player: " + (if (this.state.xIsNext) Option("X") else Option("O"))

    div(
      div(className:="status")(
        status
      ),
      div(className:="board-row")(
        renderSquare(0),
        renderSquare(1),
        renderSquare(2)
      ),
      div(className:="board-row")(
        renderSquare(3),
        renderSquare(4),
        renderSquare(5)
      ),
      div(className:="board-row")(
        renderSquare(6),
        renderSquare(7),
        renderSquare(8)
      ),
    )
  }
}

@react class Game extends StatelessComponent {
  case class Props(propValue: Unit)

  def render(): ReactElement = {
    div(className:="game")(
      div(
        MainPage()
      ),
      div(className:="game-board")(
        Board()
      ),
      div(className:="game-info")(
        div(
          CComponent(value = "Me Mario")
        ),
        div(
          ChessBoard()
        ),
        ol(

        )
      )
    )
  }
}
