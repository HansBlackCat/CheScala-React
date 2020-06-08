package com.hansblackcat.Chess
import java.io._


object mainApp extends App {
    val test = new BoardAction

    /*
    test.start("test1")
    test.debugPrintBoard
    test.debugPrintRangeAll
    */

    test.start()
    println("3e")
    println(test.currentUniShow("a8"))
    /*
    test.actWithMoveTest("e4")
    test.actWithMoveTest("e5")
    test.actWithMoveTest("Nf3")
    test.actWithMoveTest("Nc6")
    test.allHistoryMTShow()
    */
}
