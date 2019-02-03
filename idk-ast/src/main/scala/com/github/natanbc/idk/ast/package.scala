package com.github.natanbc.idk

package object ast {
    trait Node {
        def isConstant = false
    }

    class UnaryOperationNode(target: Node) extends Node {
        override def isConstant: Boolean = target.isConstant
    }

    class BinaryOperationNode(lhs: Node, rhs: Node) extends Node {
        override def isConstant: Boolean = lhs.isConstant && rhs.isConstant
    }

    case class IntConstant(i: Long) extends Node {
        override def isConstant: Boolean = true
    }

    case class FloatConstant(i: Double) extends Node {
        override def isConstant: Boolean = true
    }

    case class BooleanConstant(b: Boolean) extends Node {
        override def isConstant: Boolean = true
    }

    case class StringConstant(i: String) extends Node {
        override def isConstant: Boolean = true
    }

    case object NilConstant extends Node {
        override def isConstant: Boolean = true
    }

    case class Neg(n: Node) extends UnaryOperationNode(n)
    case class Add(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Sub(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Mul(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Div(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Mod(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Pow(a: Node, b: Node) extends BinaryOperationNode(a, b)

    case class Negate(n: Node) extends UnaryOperationNode(n)
    case class Eq(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Neq(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Greater(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class GreaterEq(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Smaller(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class SmallerEq(a: Node, b: Node) extends BinaryOperationNode(a, b)

    case class And(a: Node, b: Node) extends BinaryOperationNode(a, b)
    case class Or(a: Node, b: Node) extends BinaryOperationNode(a, b)

    case class Identifier(s: String) extends Node {
        override def isConstant: Boolean = true
    }

    case class Call(f: Node, args: List[Node]) extends Node
    case class Assign(what: Node, value: Node) extends Node

    case class If(condition: Node, ifBody: Node, elseBody: Node) extends Node {
        override def isConstant: Boolean = condition.isConstant && ifBody.isConstant && elseBody.isConstant
    }

    case class While(condition: Node, body: Node, elseBody: Node) extends Node {
        override def isConstant: Boolean = condition.isConstant && body.isConstant && elseBody.isConstant
    }

    case class Return(value: Node) extends Node

    //name may be null for closures
    case class Function(name: String, args: List[String], body: Node, varargs: Boolean, annotations: List[String]) extends Node {
        //named functions are stored in the global namespace, so they can't be removed
        //closures can
        override def isConstant: Boolean = name == null
    }

    case class Body(n: List[Node]) extends Node {
        override def isConstant: Boolean = n.forall(_.isConstant)
    }

    case class Member(key: Node, where: Node) extends Node {
        override def isConstant: Boolean = key.isConstant && where.isConstant
    }

    case class ObjectLiteral(fields: List[(Node, Node)]) extends Node {
        override def isConstant: Boolean = fields.forall(_._2.isConstant)
    }

    case class ArrayLiteral(values: List[Node]) extends Node {
        override def isConstant: Boolean = values.forall(_.isConstant)
    }

    case class Let(name: String) extends Node

    case class Global(name: String) extends Node

    case class Varargs(name: String) extends Node

    case class Unpack(v: Node) extends Node

    def simplify(node: Node, nested: Boolean = false): Node = {
        node match {
            case Neg(Neg(n)) => simplify(n)
            case Neg(IntConstant(n)) => IntConstant(-n)
            case Neg(FloatConstant(n)) => FloatConstant(-n)
            case Neg(n) =>
                if(nested) return node
                simplify(Neg(simplify(n)), nested = true)

            case Negate(Negate(n)) => simplify(n)
            case Negate(BooleanConstant(b)) => BooleanConstant(!b)
            case Negate(n) =>
                if(nested) return node
                simplify(Negate(simplify(n)), nested = true)


            case Add(StringConstant(a), StringConstant(b)) => StringConstant(a + b)
            case Add(StringConstant(a), IntConstant(b)) => StringConstant(a + b)
            case Add(StringConstant(a), FloatConstant(b)) => StringConstant(a + b)
            case Add(IntConstant(a), StringConstant(b)) => StringConstant(a + b)
            case Add(FloatConstant(a), StringConstant(b)) => StringConstant(a + b)
            case Add(n, IntConstant(0)) => simplify(n)
            case Add(n, FloatConstant(0D)) => simplify(n)
            case Add(IntConstant(0), n) => simplify(n)
            case Add(FloatConstant(0D), n) => simplify(n)
            case Add(IntConstant(a), IntConstant(b)) => IntConstant(a + b)
            case Add(FloatConstant(a), FloatConstant(b)) => FloatConstant(a + b)
            case Add(FloatConstant(a), IntConstant(b)) => FloatConstant(a + b)
            case Add(IntConstant(a), FloatConstant(b)) => FloatConstant(a + b)
            case Add(n1, n2) =>
                if(nested) return node
                simplify(Add(simplify(n1), simplify(n2)), nested = true)

            case Sub(n, IntConstant(0)) => simplify(n)
            case Sub(n, FloatConstant(0D)) => simplify(n)
            case Sub(IntConstant(0), n) => simplify(Neg(n))
            case Sub(FloatConstant(0D), n) => simplify(Neg(n))
            case Sub(IntConstant(a), IntConstant(b)) => IntConstant(a - b)
            case Sub(FloatConstant(a), FloatConstant(b)) => FloatConstant(a - b)
            case Sub(FloatConstant(a), IntConstant(b)) => FloatConstant(a - b)
            case Sub(IntConstant(a), FloatConstant(b)) => FloatConstant(a - b)
            case Sub(n1, n2) =>
                if(nested) return node
                simplify(Sub(simplify(n1), simplify(n2)), nested = true)

            case Mul(StringConstant(a), IntConstant(b)) =>
                if(b < 0 || b > Integer.MAX_VALUE) {
                    node
                } else {
                    StringConstant(a * b.toInt)
                }
            case Mul(IntConstant(0), _) => IntConstant(0)
            case Mul(FloatConstant(0D), _) => FloatConstant(0D)
            case Mul(_, IntConstant(0)) => IntConstant(0)
            case Mul(_, FloatConstant(0D)) => FloatConstant(0D)
            case Mul(IntConstant(1), n) => simplify(n)
            case Mul(FloatConstant(1D), n) => simplify(n)
            case Mul(n, IntConstant(1)) => simplify(n)
            case Mul(n, FloatConstant(1D)) => simplify(n)
            case Mul(IntConstant(a), IntConstant(b)) => IntConstant(a * b)
            case Mul(FloatConstant(a), FloatConstant(b)) => FloatConstant(a * b)
            case Mul(FloatConstant(a), IntConstant(b)) => FloatConstant(a * b)
            case Mul(IntConstant(a), FloatConstant(b)) => FloatConstant(a * b)
            case Mul(n1, n2) =>
                if(nested) return node
                simplify(Mul(simplify(n1), simplify(n2)), nested = true)

            case Div(IntConstant(0), _) => IntConstant(0)
            case Div(FloatConstant(0D), _) => FloatConstant(0D)
            case Div(n, IntConstant(1)) => simplify(n)
            case Div(n, FloatConstant(1D)) => simplify(n)
            case Div(IntConstant(a), IntConstant(b)) => IntConstant(a / b)
            case Div(FloatConstant(a), FloatConstant(b)) => FloatConstant(a / b)
            case Div(FloatConstant(a), IntConstant(b)) => FloatConstant(a / b)
            case Div(IntConstant(a), FloatConstant(b)) => FloatConstant(a / b)
            case Div(n1, n2) =>
                if(nested) return node
                simplify(Div(simplify(n1), simplify(n2)), nested = true)

            case Mod(n, IntConstant(1)) => simplify(n)
            case Mod(n, FloatConstant(1)) => simplify(n)
            case Mod(IntConstant(a), IntConstant(b)) => IntConstant(a % b)
            case Mod(FloatConstant(a), IntConstant(b)) => FloatConstant(a % b)
            case Mod(IntConstant(a), FloatConstant(b)) => FloatConstant(a % b)
            case Mod(FloatConstant(a), FloatConstant(b)) => FloatConstant(a % b)
            case Mod(a, b) =>
                if(nested) return node
                simplify(Mod(simplify(a), simplify(b)), nested = true)

            case Pow(IntConstant(0), _) => IntConstant(0)
            case Pow(FloatConstant(0D), _) => FloatConstant(0D)
            case Pow(_, IntConstant(0)) => IntConstant(1)
            case Pow(_, FloatConstant(0D)) => FloatConstant(1D)
            case Pow(n, IntConstant(1)) => simplify(n)
            case Pow(n, FloatConstant(1D)) => simplify(n)
            case Pow(IntConstant(1), _) => IntConstant(1)
            case Pow(FloatConstant(1D), _) => FloatConstant(1D)
            case Pow(IntConstant(a), IntConstant(b)) => IntConstant(Math.pow(a, b).toLong)
            case Pow(FloatConstant(a), FloatConstant(b)) => FloatConstant(Math.pow(a, b))
            case Pow(FloatConstant(a), IntConstant(b)) => FloatConstant(Math.pow(a, b))
            case Pow(IntConstant(a), FloatConstant(b)) => FloatConstant(Math.pow(a, b))
            case Pow(n1, n2) =>
                if(nested) return node
                simplify(Pow(simplify(n1), simplify(n2)), nested = true)

            case Eq(IntConstant(a), IntConstant(b)) => BooleanConstant(a == b)
            case Eq(FloatConstant(a), IntConstant(b)) => BooleanConstant(a == b)
            case Eq(IntConstant(a), FloatConstant(b)) => BooleanConstant(a == b)
            case Eq(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a == b)
            case Eq(StringConstant(a), StringConstant(b)) => BooleanConstant(a == b)
            case Eq(BooleanConstant(a), BooleanConstant(b)) => BooleanConstant(a == b)
            case Eq(IntConstant(_), n) if n.isConstant => BooleanConstant(false)
            case Eq(FloatConstant(_), n) if n.isConstant => BooleanConstant(false)
            case Eq(StringConstant(_), n) if n.isConstant => BooleanConstant(false)
            case Eq(BooleanConstant(_), n) if n.isConstant => BooleanConstant(false)
            case Eq(a, b) =>
                if(nested) return node
                simplify(Eq(simplify(a), simplify(b)), nested = true)

            case Neq(IntConstant(a), IntConstant(b)) => BooleanConstant(a != b)
            case Neq(FloatConstant(a), IntConstant(b)) => BooleanConstant(a != b)
            case Neq(IntConstant(a), FloatConstant(b)) => BooleanConstant(a != b)
            case Neq(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a != b)
            case Neq(StringConstant(a), StringConstant(b)) => BooleanConstant(a != b)
            case Neq(BooleanConstant(a), BooleanConstant(b)) => BooleanConstant(a != b)
            case Neq(IntConstant(_), n) if n.isConstant => BooleanConstant(true)
            case Neq(FloatConstant(_), n) if n.isConstant => BooleanConstant(true)
            case Neq(StringConstant(_), n) if n.isConstant => BooleanConstant(true)
            case Neq(BooleanConstant(_), n) if n.isConstant => BooleanConstant(true)
            case Neq(a, b) =>
                if(nested) return node
                simplify(Neq(simplify(a), simplify(b)), nested = true)

            case Greater(IntConstant(a), IntConstant(b)) => BooleanConstant(a > b)
            case Greater(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a > b)
            case Greater(IntConstant(a), FloatConstant(b)) => BooleanConstant(a > b)
            case Greater(FloatConstant(a), IntConstant(b)) => BooleanConstant(a > b)
            case Greater(a, b) =>
                if(nested) return node
                simplify(Greater(simplify(a), simplify(b)), nested = true)

            case GreaterEq(IntConstant(a), IntConstant(b)) => BooleanConstant(a >= b)
            case GreaterEq(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a >= b)
            case GreaterEq(IntConstant(a), FloatConstant(b)) => BooleanConstant(a >= b)
            case GreaterEq(FloatConstant(a), IntConstant(b)) => BooleanConstant(a >= b)
            case GreaterEq(a, b) =>
                if(nested) return node
                simplify(GreaterEq(simplify(a), simplify(b)), nested = true)

            case Smaller(IntConstant(a), IntConstant(b)) => BooleanConstant(a < b)
            case Smaller(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a < b)
            case Smaller(IntConstant(a), FloatConstant(b)) => BooleanConstant(a < b)
            case Smaller(FloatConstant(a), IntConstant(b)) => BooleanConstant(a < b)
            case Smaller(a, b) =>
                if(nested) return node
                simplify(Smaller(simplify(a), simplify(b)), nested = true)

            case SmallerEq(IntConstant(a), IntConstant(b)) => BooleanConstant(a <= b)
            case SmallerEq(FloatConstant(a), FloatConstant(b)) => BooleanConstant(a <= b)
            case SmallerEq(IntConstant(a), FloatConstant(b)) => BooleanConstant(a <= b)
            case SmallerEq(FloatConstant(a), IntConstant(b)) => BooleanConstant(a <= b)
            case SmallerEq(a, b) =>
                if(nested) return node
                simplify(SmallerEq(simplify(a), simplify(b)), nested = true)

            case And(BooleanConstant(false), _)=> BooleanConstant(false)
            case And(NilConstant, _)=> NilConstant
            case And(a, b) =>
                if(nested) return node
                simplify(And(simplify(a), simplify(b)), nested = true)

            case Or(BooleanConstant(true), _) => BooleanConstant(true)
            case Or(BooleanConstant(false), b) => simplify(b)
            case Or(a, b) =>
                if(nested) return node
                simplify(Or(simplify(a), simplify(b)), nested = true)

            case If(BooleanConstant(true), a, _) => simplify(a)
            case If(BooleanConstant(false), _, a) => simplify(a)
            case If(a, b, c) =>
                if(nested) return node
                simplify(If(simplify(a), simplify(b), simplify(c)), nested = true)

            case While(BooleanConstant(false), _, e) => simplify(e)
            case While(a, b, e) =>
                if(nested) return node
                simplify(While(simplify(a), simplify(b), simplify(e)), nested = true)

            case Body(b) if b.lengthCompare(1) == 0 => simplify(b.head)
            case Body(b) =>
                if(nested) return node
                simplify(Body(b.map(simplify(_)).filter({
                    case n if n.isConstant && (n ne b.last) => false
                    case _ => true
                })), nested = true)

            case Function(name,args,body,varargs,annotations) =>
                if(nested) return node
                simplify(Function(name, args, simplify(body), varargs, annotations), nested = true)

            case Assign(a, b) =>
                if(nested) return node
                simplify(Assign(simplify(a), simplify(b)), nested = true)

            case Member(IntConstant(n), ArrayLiteral(values)) =>
                if(values.forall(_.isConstant)) {
                    if(values.size < n || n < 0) {
                        NilConstant
                    } else {
                        simplify(values(n.toInt))
                    }
                } else {
                    if(nested) return node
                    val simplified = values.map(simplify(_))
                    if(simplified.size > n && n >= 0) {
                        simplify(Member(IntConstant(n), ArrayLiteral(simplified)), nested = true)
                    } else {
                        simplify(Body(simplified :+ NilConstant))
                    }
                }
            case Member(key, ArrayLiteral(values)) if key.isConstant && values.forall(_.isConstant) =>
                NilConstant
            case Member(StringConstant(s), ObjectLiteral(fields)) =>
                if(fields.forall(_._2.isConstant)) {
                    fields.find(_._1 == Identifier(s)) match {
                        case Some(p) => simplify(p._2)
                        case None => NilConstant
                    }
                } else {
                    if(nested) return node
                    val simplified = fields.map(p=>(p._1, simplify(p._2)))
                    if(simplified.exists(_._1 == Identifier(s))) {
                        simplify(Member(StringConstant(s), ObjectLiteral(simplified.filter {
                            case (Identifier(`s`), _) => true
                            case (_, v) if v.isConstant => false
                            case _ => true
                        })), nested = true)
                    } else {
                        simplify(Body(simplified.map(_._2).filter(n => !n.isConstant) :+ NilConstant))
                    }
                }

            case Member(IntConstant(n), StringConstant(s)) =>
                if(n >= s.length) {
                    NilConstant
                } else if(n >= 0) {
                    StringConstant(s.charAt(n.toInt).toString)
                } else {
                    node
                }

            case Member(a, b) =>
                if(nested) return node
                simplify(Member(simplify(a), simplify(b)), nested = true)

            case ObjectLiteral(fields) =>
                if(nested) return node
                simplify(ObjectLiteral(fields.map(p=>(p._1, simplify(p._2)))), nested = true)

            case ArrayLiteral(values) =>
                if(nested) return node
                simplify(ArrayLiteral(values.map(simplify(_))), nested = true)

            case Call(f, args) => Call(simplify(f), args.map(simplify(_)))

            case Return(v) => Return(simplify(v))

            case n => n
        }
    }
}
