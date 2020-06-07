package com.hansblackcatnas

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import org.scalajs.dom
import slinky.core.facade.ReactElement
import scala.scalajs.js.annotation.JSExportTopLevel



@react class CComponent extends StatelessComponent {
    case class Props(value: String)

    def render(): ReactElement = {
        val this_props = this.props

        return (
            div(
                b(s"Obj Val: ${this_props.value}")
            )
        )
    }
}