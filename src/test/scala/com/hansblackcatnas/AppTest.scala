package com.hansblackcatnas

import slinky.web.ReactDOM
import org.scalajs.dom.document

import org.scalatest.FunSuite


class AppTest extends FunSuite {
  test("Renders without crashing") {
    val div = document.createElement("div")
    ReactDOM.render(ChessBoard(), div)
    ReactDOM.unmountComponentAtNode(div)
  }
}
