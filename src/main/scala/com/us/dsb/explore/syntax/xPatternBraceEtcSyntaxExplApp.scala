package com.us.dsb.explore.syntax


object xPatternBraceEtcSyntaxExplApp extends App {



  case class C(a: Int, b: Int)
  class B
  case class D1(x: Any) extends B
  case class D2(x: Any) extends B

  val c1 = List(new B, D1(1), D1("2"), D2(1))
  val c2 = List((1, 101), (2, "1 0 2"), 3, (4, 404, "FOUR"))


  c1.foreach _
  if (false) {
    c1.foreach(null)
    c1.foreach{null}
  }


  c1  .foreach(v => println)
  //c1.foreach(v => println; println)        // syntax: expected ")" at the ";"
  c1  .foreach(v => {println; println})
  //c1.foreach(v => (println; println))      // syntax: expected ")" at the ";"

  //c1.foreach(v: B => println)             // ~syntax: "not found: type println"
  //c1.foreach(v: B => println; println)    // syntax: expected ")" at the ";"
  //c1.foreach(v: B => {println; println})  // syntax: ill. start of decl. at first "println"
  //c1.foreach(v: B => (println; println))  // syntax: expected ")" at the ";"

  c1  .foreach((v: B) => println)
  //c1.foreach((v: B) => println; println)    // syntax: expected ")" at the ";"
  c1  .foreach((v: B) => {println; println})
  //c1.foreach((v: B) => (println; println))  // syntax: expected ")" at the ";"

  //c1.foreach((v: D1) => println)             // type mismatch; was D1 => Unit,  req'd: B => ?
  //c1.foreach((v: D1) => println; println)    // syntax: expected ")" at the ";"
  //c1.foreach((v: D1) => {println; println})  // type mismatch; was D1 => Unit,  req'd: B => ?
  //c1.foreach((v: D1) => (println; println))  // syntax: expected ")" at the ";"


  c1.foreach(v => {println; println})
  c1.foreach((v: B) => {println; println})
  //c1.foreach((v: D1) => {println; println})  // type mismatch; was D1 => Unit,  req'd: B => ?
  c1.foreach({v => println; println})
  c1.foreach({v: B => println; println})
  //c1.foreach({v: D1 => println; println})  // type mismatch; was D1 => Unit,  req'd: B => ?

  c1.foreach{v => {println; println}}
  c1.foreach{(v: B) => {println; println}}
  //c1.foreach{(v: D1) => {println; println}}  // type mismatch; was D1 => Unit,  req'd: B => ?
  c1.foreach{{v => println; println}}
  c1.foreach{{v: B => println; println}}
  //c1.foreach{{v: D1 => println; println}}  // type mismatch; was D1 => Unit,  req'd: B => ?


  /*
- Braces/parentheses/patterns/function literals after, e.g,. .map
*/




  /*
    Patterns in value/variable declarations/definitions:

    - Dcl and Def down to PatDef (to Pattern2):

    Dcl          ::=  ‘val’ ValDcl
                   |  ‘var’ VarDcl
                   |  ...
    ValDcl       ::=  ids ‘:’ Type
    VarDcl       ::=  ids ‘:’ Type
    ids          ::=  id {‘,’ id}

    Def          ::=  PatVarDef               -- ('val' or 'var')
                   |  ...

    PatVarDef    ::=  ‘val’ PatDef
                   |  ‘var’ VarDef

    VarDef       ::=  PatDef                  -- parallel to 'val' PatDef
                   |  ids ‘:’ Type ‘=’ ‘_’    -- (default initial value)


    PatDef       ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
                      -- "enters" patterns below Pattern and Patternl (whose "or"
                      --   and wildcard/boundvarid make no sense _at_the_top_level_
                      --   for declarations), _but_ ... nested ...

    - Patterns:

    Pattern         ::=  Pattern1 { ‘|’ Pattern1 }
    Pattern1        ::=  boundvarid ‘:’ TypePat
                      |  ‘_’ ‘:’ TypePat
                      |  Pattern2
    Pattern2        ::=  id [‘@’ Pattern3]
                      |  Pattern3
    Pattern3        ::=  SimplePattern
                      |  SimplePattern {id [nl] SimplePattern}
    SimplePattern   ::=  ‘_’
                      |  varid
                      |  Literal
                      |  StableId
                      |  StableId ‘(’ [Patterns] ‘)’
                      |  StableId ‘(’ [Patterns ‘,’] [id ‘@’] ‘_’ ‘*’ ‘)’
                      |  ‘(’ [Patterns] ‘)’
                      |  XmlPattern
    Patterns        ::=  Pattern {‘,’ Patterns}

    TypePat         ::=  Type


    varid           ::=  lower idrest
    Literal         ::= ...
    Type            ::= ....

    StableId        ::=  id
                      |  Path ‘.’ id
                      |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
    Path            ::=  StableId
                      |  [id ‘.’] this
    ClassQualifier  ::= ‘[’ id ‘]’
  */




}
