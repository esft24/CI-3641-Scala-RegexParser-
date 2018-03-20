import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader
import scala.io.StdIn

/*
    Parser e interprete para una calculadora simple que acepta las
    operaciones de suma, resta, multiplicacion, division y modulo 
    sobre numeros decimales. Creada mediante el uso de las librerias
    de parser combinators 

    Ademas se agrego un REPL para probar esta calculadora facilmente
*/

// trait y case clases que definen un pequeno arbol sintactico que
// contendra y dara un contexto a cualquier elemento capturado por
// nuestro parser.
sealed trait Expr
case class Numero(valor: Double) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Subs(left: Expr, right: Expr) extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Pow(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr

//sealed traits para los operadores, ayudara al pattern matching a
//darse cuenta que no debe checkear otras strings mas que las de los
//operadores
sealed trait OperatorMP
case object Minus extends OperatorMP
case object Plus extends OperatorMP

sealed trait OperatorDMM
case object Divider    extends OperatorDMM 
case object Module     extends OperatorDMM
case object Multiplier extends OperatorDMM

/*
    Notacion de la gramatica libre de contexo usada
       Uso         Notacion
    definicion        =
    alternacion       |
    opcional(0-1)   [...] 
    repeticion(0-n) {...}
    concatenacion     ~

    EBNF de nuestra calculadora:
    decimalNumber es definido por la expresion regular
    "[0-9]"+ ("." "[0-9]"*)? | "[0-9]"* "." "[0-9]"+
    con + notacion para 1 o mas, ? para 0 o 1 y | para alternacion.
    
    expr   = term ~ {"+"|"-" ~ term}
    term   = power ~ ["%"|"/"|"*" ~ term]
    power  = factor ~ ["^" ~ power]
    factor = decimalNumber | "(" expr ")"
*/

// Parser de nuestra calculadora, hereda del trait RegexParsers y se 
// la clase PackratParsers como un mix-in.
//
// RegexParsers es un trait que permite la creacion de un parser
// basado en expresiones regulares para su lexing.
//
// La gramatica de nuestro programa sera traducida a una serie de
// definiciones de funciones con el mismo nombre, las cuales retornaran
// un objeto de tipo parser.
// 
// Este objeto consumira una entrada mediante la funcion "parseAll",
// dandole a este una de las funciones parseadoras (la mayoria de
// las veces sera la funcion expr), y usara funciones importadas
// del mismo trait (~, [], ^^...), creando con estas un objeto con
// valores de tipo de nuestro AST creado anteriormente.
//
// El trait PackratParsers provee un algoritmo mas eficiente de parseo
// el cual usa un metodo de cacheo de resultados viejos. Este asegura
// que el tiempo de parseo sea proporcional al largo de la entrada y
// (aunque no es relevante) permite gramaticas recursivas a la izquierda.
//
// Este mix-in es solo un agregado a las funciones de RegexParsers,
// es posible hacer este mismo parser sin necesidad del uso de 
// PackratParsers. El uso de valores lazy en vez de definiciones 
// de funciones para las reglas gramaticales es necesario para que la tecnica
// de cacheo sea posible.
class Calculadora extends RegexParsers with PackratParsers {
    // Expresion regular que captura numeros decimales
    val decimalNumber = "\\d+(\\.\\d*)?|\\d*\\.\\d+".r
    
    // Cada regla gramatical usa ciertas funciones de nivel superior llamadas
    // parser combinators, estas son la base del funcionamiento del paquete
    // de parsing de Scala. Estas se encargan de tomar varios parsers para 
    // devolver uno nuevo, el cual sera el que finalmente tome a la string
    // que sera parseada para devolver el objeto que queremos crear.

    // ~   wrapper sobre una secuencia de matches, permite la concatenacion.
    // rep Generador de parsers para repeticiones.
    // opt Generador de parsers para frases opcionales.
    // (a | b)  Si el parser a no resulta en un match, intenta con b.

    // Los operadores ^^ y ^^^ nos permiten transformar resultados de un 
    // parser en lo que queramos. ^^ nos permite usar pattern matching mientras
    // que ^^^ es usado cuando se quiere cambiar el resultado por un valor constante.

    // regla: expr = term ~ {"+"|"-" ~ term}
    lazy val expr: PackratParser[Expr] = term ~ rep( ("+" ^^^ Plus | "-" ^^^ Minus) ~ term) ^^ {
        case t ~ r => r.foldLeft(t)(  // Usamos un fold
            (acc, ter) => ter match {
                case Plus ~ n  => Add(acc, n)
                case Minus ~ n => Subs(acc, n)
            }
        )
    }

    // regla term   = power ~ ["%"|"/"|"*" ~ term]
    lazy val term: PackratParser[Expr] = power ~ opt(("%"^^^Module|"/"^^^Divider|"*"^^^Multiplier) ~ term) ^^ {
        case p ~ None         => p
        case p ~ Some(e) => e match {
            case Divider ~ t    => Div(p, t)
            case Multiplier ~ t => Mult(p, t) 
            case Module ~ t     => Mod(p, t) 
        }  
    }
    
    // regla power  = factor ~ ["^" ~ power]
    lazy val power: PackratParser[Expr] = factor ~ opt("^" ~> power) ^^ {
        case f ~ None    => f
        case f ~ Some(p) => Pow(f, p)
    }

    // regla factor = decimalNumber | "(" expr ")"
    lazy val factor: PackratParser[Expr] = decimalNumber^^{n => Numero(n.toDouble)} |
                                           "(" ~> expr <~ ")"

    // Redefinicion de la funcion parseAll, RegexParsers la tiene pero PackratParsers no,
    // por alguna razon.
    // Devuelve un objeto que determina si el parseo fue un exito o no (Success y NoSuccess) 
    // junto a otro objeto de tipo Expr o error respectivamente. se puede conseguir con .get
    // o mediante pattern matching
    def parseAll[T](p: Parser[T], input: String) =
        phrase(p)(new PackratReader(new CharSequenceReader(input)))

}

// Objeto Singleton Interprete que convertira a una expresion Expr en
// lo que esta representaria en Scala.
object Interprete {
    // Funcion de nombre apply nos deja usar el interprete como si fuera solo
    // una funcion.
    // Dada una expresion, consigue mediante pattern matching cual es y 
    // devuelve su valor numerico.
    def apply(expr: Expr): Double = {
        expr match {
            case Numero(v)  => v
            case Add(l, r)  => apply(l) + apply(r)
            case Subs(l, r) => apply(l) - apply(r)
            case Mult(l, r) => apply(l) * apply(r)
            case Div(l, r)  => apply(l) / apply(r)
            case Mod(l, r)  => apply(l) % apply(r)
            case Pow(b, e)  => Math.pow(apply(b),apply(e))
        }
    }
}

// Objeto Singleton que actua como un ambiente REPL para probar facilmente
// la funcionalidad de nuestra calculadora.
object CalculadoraREPL{
    val parser = new Calculadora
    def main(args: Array[String]) = loop()

    def loop() {
        while (true) {
            val exprSrc = StdIn.readLine("Calculame Este> ")
            if (exprSrc == "") return
            import parser.{Success, NoSuccess}
            parser.parseAll(parser.expr, exprSrc) match {
                case Success(expr, _) => println(Interprete(expr))
                case err: NoSuccess   => println(err)
            }
        }
    }
}