package com.github.natanbc.idk

import java.io.File

import com.github.natanbc.idk.parser.Parser
import com.github.natanbc.idk.parser.parselets._
import com.github.natanbc.idk.runtime._
import com.github.natanbc.idk.compiler.Compiler

import scala.collection.mutable
import scala.io.{Source, StdIn}

object IDK extends App {
    import ast._

    var running = true
    var debug = false
    var speedtest = false
    var compilerSpeedTest = false


    val vars = new mutable.HashMap[Value, Value]()

    StdLib.install(vars)

    class Thing {
        var myInt: Int = 1
        def myMethod(): Unit = println("myMethod() called")

        def myMethod(i: Int): Unit = println(s"myMethod(Int) called, i = $i")
    }

    vars(StringValue("test")) = Value.of(new Thing())

    println("Type -help for a list of commands")
    while(running) {
        val line = StdIn.readLine("input> ")
        if(line == "-stop") {
            running = false
        } else if(line == "-help") {
            println(
                """
                  |-stop: quit the program
                  |-debug on: print raw and optimized ASTs plus bytecode
                  |-debug off: disable printing of debug information
                  |-run <file>: evaluate code in a file
                  |-vars: print variables
                  |-speedtest <+|-flag>: enable/disable a speed test
                  |     Valid tests:
                  |         compiler
                  |         execution
                """.stripMargin)
        } else if(line.startsWith("-speedtest")) {
            if(line == "-speedtest") {
                println("Missing mode and flag")
            } else {
                val f = line.substring(10).trim
                val v = if(f.startsWith("+")) {
                    1
                } else if(f.startsWith("-")) {
                    0
                } else {
                    println("Invalid mode: must be either + or -")
                    -1
                }
                if(v != -1) {
                    f.substring(1) match {
                        case "compiler" => compilerSpeedTest = v == 1
                        case "execution" => speedtest = v == 1
                        case _ => println("Unknown speedtest mode")
                    }
                }
            }
        } else if(line == "-debug on") {
            debug = true
        } else if(line == "-debug off") {
            debug = false
        } else if(line.startsWith("-run ")) {
            val name = line.substring(5)
            var f = new File(name)
            if(!f.exists()) {
                f = new File(name + ".idk")
            }
            if(!f.exists()) {
                println(s"File not found:\n\tTried $name\n\tTried $name.idk")
            } else {
                val s = Source.fromFile(f)
                val code = s.mkString
                s.close()
                execute(code)
            }
        } else if(line == "-vars") {
            vars.foreach(p=>{
                print(p._1)
                print(" = ")
                println(p._2)
            })
        } else {
            execute(line)
        }
    }

    private def execute(code: String): Unit = {
        try {
            val rawInterpreter = new Interpreter(new Compiler(code, simplify = false).compile(), vars)
            val simplifiedInterpreter = new Interpreter(new Compiler(code).compile(), vars)


            val parser = new Parser(code)
            register(parser)
            val raw = new collection.mutable.ListBuffer[Node]()
            var node = parser.parseExpression()
            while(node != null) {
                raw += node
                node = parser.parseExpression()
            }
            val simple = raw.map(simplify(_))

            if(debug) {
                println("Raw AST:       " + raw.mkString(","))
                println("Optimized AST: " + simple.mkString(","))
                println("Bytecode:")
                Compiler.printCode(new Compiler(code).compile())
                println("Result:        " + simplifiedInterpreter.run())
            } else {
                println("Result: " + simplifiedInterpreter.run())
            }

            if(compilerSpeedTest) {
                val rawCompileTime = avg(10000) {
                    new Compiler(code, simplify = false).compile()
                }

                val simpleCompileTime = avg(10000) {
                    new Compiler(code, simplify = true).compile()
                }

                println("Raw AST compile:        " + rawCompileTime + " ns/run")
                println("Optimized AST compile:  " + simpleCompileTime + " ns/run")
            }

            if(speedtest) {
                val avgRaw = avg(100000) {
                    rawInterpreter.run()
                }

                val avgSimple = avg(100000) {
                    simplifiedInterpreter.run()
                }

                println("Raw AST eval:        " + avgRaw + " ns/run")
                println("Optimized AST eval:  " + avgSimple + " ns/run")
            }
        } catch {
            case e: ExecutionException =>
                if(e.callStack == null) {
                    e.printStackTrace()
                } else {
                    Console.err.println(e.getMessage)
                    e.callStack.foreach(f=>{
                        Console.err.println(s"\tat $f")
                    })
                }
                Thread.sleep(10)
            case t: Throwable =>
                if(debug) {
                    t.printStackTrace()
                } else {
                    println(t)
                }
                Thread.sleep(10)
        }

    }

    def avg(runs: Int)(block: =>Unit): Long = {
        val now = System.nanoTime()
        (1 to runs).foreach(_=>block)
        (System.nanoTime() - now) / runs
    }
}
