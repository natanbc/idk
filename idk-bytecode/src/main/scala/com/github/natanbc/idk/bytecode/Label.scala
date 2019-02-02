package com.github.natanbc.idk.bytecode

class Label private[bytecode]() {
    private[bytecode] var written = false
    private[bytecode] var index = 0
}
