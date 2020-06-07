package com.hansblackcatnas

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom
import slinky.core.facade.ReactElement
import scala.scalajs.js.annotation.JSExportTopLevel

@react class MainPage extends StatelessComponent {
    type Props = Unit

    def render(): ReactElement = {
        div(
            "Go Work"
        )
    }
}
