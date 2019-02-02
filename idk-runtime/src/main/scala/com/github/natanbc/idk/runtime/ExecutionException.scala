package com.github.natanbc.idk.runtime

class ExecutionException(msg: String) extends RuntimeException(msg) {
    private var _callStack: List[String] = _

    def callStack_=(c: List[String]): Unit = {
        if(_callStack != null) throw new IllegalStateException("Call stack already set")
        _callStack = c
    }

    def callStack: List[String] = _callStack
}
