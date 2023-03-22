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

let jump (Exit n) = ;;

type instruction =Inc of ((int Register.t)->label->(int Register.t)) 
                  | Dec of ((int Register.t)->label->(int Register.t)) 
                  | Clear of ((int Register.t)->label->(int Register.t)) 
                  | Copy of ((int Register.t) ->label->label->(int Register.t))
                  | Jump of (exit-> instruction)
                  | JumpM of (label->exit->instruction) ;;


type param = LabelParam of label 
            | LabelCouple of (label*label) 
            | Exitparam of exit
            | LabelExCouple of (label*exit)
;;

type programm = Programm of (instruction*param) list;;


let execution (Programm programme) (Register registre) = ;;
