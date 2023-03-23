(*Magine à registre Axel Verneuil, Valentin Guy-Deroubaix*)


type label = Label of int;; (*name of the register*)

(*type register = Register of {label:int; integer : int} ;;*)

let compare_registerLabel (Label n) (Label m) = 
  if n > m
    then 
        1
else 
    if n = m
        then 
            0
        else 
            -1
;;


module RegistersOrdonee = struct
  type t = label;;
  let compare = compare_registerLabel;;
end

module Register = Map.Make(RegistersOrdonee)

type exit = Exit of int ;; (*ligne number*)

let inc registers n = (Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some (e+1))) registers)                            
;;

let dec registers n = if (Register.find n registers) > 0 
                        then 
                            (Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some (e-1))) registers) 
                        else registers
;;
let clear registers n = (Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some 0)) registers)                            
;;

let jump (Exit n) = (Exit n) ;;

let jumpM register m (Exit n) = if (Register.find m register) = 0 then n else  ;;

type param = LabelParam of ((int Register.t)*label) 
            | LabelCouple of ((int Register.t)*label*label) 
            | Exitparam of exit
            | LabelExCouple of (label*exit)
;;

type instruction = Mono1S of (LabelParam->(int Register.t)) (*un parametre une sortie*)
                  | Duo1S of (LabelCouple->(int Register.t)) (*deux parametres une sortie*)
                  | Mono2S of (Exitparam->exit) (*un parametre deux sorties*)
                  | Duo2S of (LabelExCouple->exit) (*deux parametres deux sorties*)
;;


type programm = Programm of (instruction*param) list;;


let rec execution (Programm programme) registre = 
  match programme with 
    | [] -> []
    | (instruction,param)::t -> begin match instruction with
                                  | Mono1S(instruction) -> 
                                  | Duo1S(instruction)
                                  | Mono2S(instruction)
                                  | Duo2S(instruction)

                                   
;;
