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

@JSImport("resources/ChessBoard.css", JSImport.Default) 
@js.native
object ChessBoardCSS extends js.Object

@react class ChessBoard extends StatelessComponent {
    type Props = Unit

    private val css = ChessBoardCSS

    def render(): ReactElement = {
        div(
            header(
                h5(className := "Head")("Welcome to CheScala")
            ),
            p(
                StateExample()
            )
        )
    }
}


// Example
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


