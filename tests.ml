let string1 = "let x = 5 in x * x" 
let string2 = "fun (x: int) -> x" 
let string3 = "let rec fac (a: int): int = fun (n: int) -> 
    if n <= 1 then a else fac (n*a) (n-1) in fac 1 5"
let string4 = "1+"
let string5 = "if x then y else z"
let string6 = "1if"
let string7 = "fun x:int -> x"

let lex1 = lex string1 
let lex2 = lex string2 
let lex3 = lex string3 
let lex4 = lex string4 
let lex5 = lex string5 
let lex6 = lex string6 
let lex7 = lex string7 

let parse1 = exp lex1 
let parse2 = exp lex2 
let parse3 = exp lex3 
let parse4 = exp lex4 
let parse5 = exp lex5 
let parse6 = exp lex6 
let parse7 = exp lex7 

let type1 = checkStr string1 
let type2 = checkStr string2 
let type3 = checkStr string3 
let type4 = checkStr string4 
let type5 = checkStr string5 
let type6 = checkStr string6 
let type7 = checkStr string7 

let value1 = evalStr string1 
let value2 = evalStr string2 
let value3 = evalStr string3  
let value4 = evalStr string4 
let value5 = evalStr string5 
let value6 = evalStr string6 
let value7 = evalStr string7 