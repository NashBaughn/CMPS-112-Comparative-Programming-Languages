(* 
nbaughn@ucsc.edu
Nash Baughn
CS 112 Sping Quarter 2017 *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

let aget = Array.get
let aset = Array.set
let amake = Array.make
let ord = Char.code

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let symbol_table = amake 256 (false, 0.0)

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let print_number number = 
    let number' = string_of_bigint number in
    let length = String.length number' in
    let rec print_number' index =
        if index >= length then
            printf "\n%!"
        else 
            match number'.[index] with
            | c -> 
                if index > 69 && (index) mod 70 == 0 then
                    printf "\\\n%c" c
                else
                    printf "%c%!" c; print_number' (index+1)  
                
    in print_number' 0


let print_stackempty () = printf "stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> push (aget symbol_table reg) thestack 
        | 's' -> aset symbol_table reg (true, pop thestack)
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack 
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> executereg thestack oper reg
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | 's'  -> executereg thestack oper reg
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "End_of_file\n%!";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
