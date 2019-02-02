package com.github.natanbc.idk.compiler

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream}

import com.github.natanbc.idk.ast._
import com.github.natanbc.idk.bytecode.{BytecodeWriter, FunctionWriter, Opcodes}
import com.github.natanbc.idk.lexer.SyntaxException
import com.github.natanbc.idk.parser.{Parser, parselets}

class Compiler(parser: Parser, simplify: Boolean) {
    def this(parser: Parser) = {
        this(parser, true)
    }

    def this(code: String, simplify: Boolean) = {
        this(new Parser(code), simplify)
        parselets.register(parser)
    }

    def this(code: String) = {
        this(code, true)
    }

    private val out = new ByteArrayOutputStream()
    private val bw = new BytecodeWriter(out)
    private var nextFunctionId = 0

    def compile(): Array[Byte] = {
        val mainFw = bw.newFunction(0, "$main", List(), varargs = false, List())
        var node = parser.parseExpression()
        while(node != null) {
            val next = parser.parseExpression()
            compile(if(simplify) {
                com.github.natanbc.idk.ast.simplify(node)
            } else {
                node
            }, mainFw, next == null)
            node = next
        }
        mainFw.write()
        bw.write()
        out.toByteArray
    }

    private def compile(f: Node, fw: FunctionWriter, resultUsed: Boolean = true): Unit = {
        f match {
            case IntConstant(v) =>      fw.loadConstant(v)
            case FloatConstant(v) =>    fw.loadConstant(v)
            case StringConstant(v) =>   fw.loadConstant(v)
            case BooleanConstant(v) =>  fw.loadConstant(v)
            case NilConstant =>         fw.loadNil()

            case Add(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.ADD)
            case Sub(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.SUB)
            case Mul(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.MUL)
            case Div(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.DIV)
            case Mod(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.MOD)
            case Pow(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.POW)

            //-x
            case Neg(x) =>
                compile(x, fw)
                fw.writeInstruction(Opcodes.NEG)

            //!x
            case Negate(x) =>
                compile(x, fw)
                fw.writeInstruction(Opcodes.NEGATE)

            case Eq(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.EQ)

            case Neq(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.NEQ)

            case Greater(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.GREATER)

            case GreaterEq(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.GREATER_EQ)

            case Smaller(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.SMALLER)

            case SmallerEq(a, b) =>
                compile(a, fw)
                compile(b, fw)
                fw.writeInstruction(Opcodes.SMALLER_EQ)

            case And(a, b) =>
                compile(a, fw)
                val l = fw.newLabel()
                fw.dup()
                fw.ifFalse(l)
                compile(b, fw)
                fw.writeInstruction(Opcodes.AND)
                fw.writeLabel(l)

            case Or(a, b) =>
                compile(a, fw)
                val l = fw.newLabel()
                fw.dup()
                fw.ifTrue(l)
                compile(b, fw)
                fw.writeInstruction(Opcodes.OR)
                fw.writeLabel(l)

            case Call(a, args) if args.lastOption.exists(_.isInstanceOf[Unpack]) =>
                compile(a, fw)
                args.map({
                    case Unpack(v) => v
                    case v => v
                }).foreach(compile(_, fw))
                fw.unpackCall(args.length)

            case Call(a, args) =>
                compile(a, fw)
                args.foreach(compile(_, fw))
                fw.call(args.length)

            case Body(nodes) =>
                nodes.zipWithIndex.foreach {
                    case (n, i) => compile(n, fw,
                        resultUsed && nodes.lengthCompare(i + 1) == 0)
                }

            case Identifier(name) =>
                fw.loadVariable(name)
            case Assign(Identifier(name), value) =>
                compile(value, fw)
                fw.storeVariable(name)
            case Assign(Member(key, where), value) =>
                compile(where, fw)
                compile(key, fw)
                compile(value, fw)
                fw.storeMember()
            case Assign(Let(name), value) =>
                fw.declareLocal(name)
                compile(value, fw)
                fw.storeVariable(name)
            case Assign(Global(name), value) =>
                compile(value, fw)
                fw.storeGlobal(name)
            case Let(name) =>
                fw.declareLocal(name)

            case Member(field, where) =>
                compile(where, fw)
                compile(field, fw)
                fw.loadMember()

            case If(condition, ifBody, elseBody) =>
                val l1 = fw.newLabel()
                val l2 = fw.newLabel()

                compile(condition, fw)
                fw.ifFalse(l1)
                compile(ifBody, fw, resultUsed)
                fw.jumpTo(l2)
                fw.writeLabel(l1)
                compile(elseBody, fw, resultUsed)
                fw.writeLabel(l2)

            case While(condition, body) =>
                val l1 = fw.newLabel()
                val l2 = fw.newLabel()

                fw.writeLabel(l1)
                compile(condition, fw)
                fw.ifFalse(l2)
                compile(body, fw, resultUsed = false)
                fw.jumpTo(l1)
                fw.writeLabel(l2)
                fw.loadNil()

            case Return(value) =>
                compile(value, fw)
                fw.ret()

            case ObjectLiteral(elements) =>
                for((k,v) <- elements) {
                    fw.loadConstant(k)
                    compile(v, fw)
                }
                fw.objectLiteral(elements.length)

            case ArrayLiteral(values) =>
                for(n <- values) {
                    compile(n, fw)
                }
                fw.arrayLiteral(values.length)

            case Unpack(_) => throw new SyntaxException("Unpack outside function call")

            case Function(name, args, code, varargs, annotations) =>
                nextFunctionId += 1
                val id = nextFunctionId
                val fw2 = bw.newFunction(id, name, args, varargs, annotations, fw)
                compile(code, fw2)
                fw2.write()
                val decorators = annotations.filter(_.startsWith("@")).map(_.substring(1)).reverse
                decorators.foreach(d=>{
                    fw.loadVariable(d)
                })
                fw.writeInstruction(Opcodes.LOAD_FUNCTION, id)
                for(name <- decorators) {
                    fw.decoratorCall(name)
                }
                if(name != null) {
                    fw.storeGlobal(name)
                }
        }
        if(!resultUsed) fw.pop()
    }
}

object Compiler {
    def printCode(code: Array[Byte]): Unit = {
        val in = new DataInputStream(new ByteArrayInputStream(code))

        val cp = new collection.mutable.ListBuffer[Any]()
        val cpLength = in.readUnsignedShort()
        println(s"Constant pool (size = $cpLength):")
        for(i <- 1 to cpLength) {
            print(s"  ${i - 1}: ")
            in.readUnsignedByte() match {
                case Opcodes.Meta.CONSTANT_STRING =>
                    val v = in.readUTF()
                    cp += v
                    println(s"String $v")
                case Opcodes.Meta.CONSTANT_LONG =>
                    val v = in.readLong()
                    cp += v
                    println(s"Integer $v")
                case Opcodes.Meta.CONSTANT_DOUBLE =>
                    val v = in.readDouble()
                    cp += v
                    println(s"Float $v")
            }
        }
        in.readInt()
        while(in.available() > 0) {
            in.readUnsignedByte() match {
                case Opcodes.Meta.FUNCTION =>
                    print(s"Function (id = ${in.readUnsignedShort()})")
                    if(in.readBoolean()) print(s" ${cp(in.readUnsignedShort())}")
                    val varargs = in.readBoolean()
                    val annotationCount = in.readUnsignedShort()
                    if(annotationCount > 0) {
                        print("[")
                        for(i <- 1 to annotationCount) {
                            print(s"${cp(in.readUnsignedShort())}")
                            if(i < annotationCount) print(", ")
                        }
                        print("]")
                    }
                    val args = in.readUnsignedShort()
                    print("(")
                    for(i <- 1 to args) {
                        print(s"${cp(in.readUnsignedShort())}")
                        if(i < args) print(", ")
                    }
                    println(")")
                    in.readUnsignedShort()
                    val codeSize = in.readUnsignedShort()
                    val bytes = new Array[Byte](codeSize)
                    in.readFully(bytes)
                    printInstructions(cp, bytes)
            }
        }
    }

    private def printInstructions(cp: collection.mutable.ListBuffer[Any], i: Array[Byte]): Unit = {
        val in = new DataInputStream(new ByteArrayInputStream(i))
        var idx = 0
        while(in.available() > 0) {
            val opcode = Opcodes.fromID(in.readUnsignedByte())
            print(s"    $idx: " + Opcodes.nameOf(opcode.id))
            for(_ <- 1 to opcode.narg) {
                print(" " + in.readUnsignedShort())
            }
            println()
            idx += 1
        }
    }
}
