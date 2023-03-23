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

(*-------------------------------------------------------------------------------------------------*)
(* Création d'un Map pour avoir une valeur non-négative entiere par une clé representant le n registre*)
module RegistersOrdonee = struct
  type t = label;;
  let compare = compare_registerLabel;;
end

module Register = Map.Make(RegistersOrdonee)
(*-----------------------------------------------------------------------------------------------*)
(*Création d'un type exit pour indiquer à quelle ligne il faut revenir*)
type exit = Exit of int ;; (*ligne number*)

(*------------------------------------------------------------------------------------------------*)
(*Création du sets d'instructions de L'URM*)
let inc registers n  = (Register.update n (fun e -> match e with
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

let jumpM register m (Exit n) = if (Register.find m register) = 0 then n else (* TODO: dans ce cas la on passe à l'instruction suivante sans rien faire*) ;;

(*TODO: faire l'instruction copy*)

(*------------------------------------------------------------------------------------------------*)
(*Création du type param pour les différents paramètres. On créer également un type instuction car les instructions n'ont pas le même nombre de sortie*)

type param = LabelParam of ((int Register.t)*label) (*Pour Inc et Dec*)
            | LabelCouple of ((int Register.t)*label*label) (*Pour copy*) 
            | Exitparam of exit (*Pour jump*)
            | LabelExCouple of (label*exit) (*Pour jumpM*) 
;;

type instruction = Mono1S_Inc of (LabelParam->(int Register.t))
		  |Mono1_Dec of (LabelParam->(int Register.t)) (*un parametre une sortie*)
                  | Duo1S of (LabelCouple->(int Register.t)) (*deux parametres une sortie*)
                  | Mono2S of (Exitparam->exit) (*un parametre deux sorties*)
                  | Duo2S of (LabelExCouple->exit) (*deux parametres deux sorties*)
;;
(*----------------------------------------------------------------------------------------------*)
(* On passe à la création de nos opérations arithmétiques. Pour cela, on créer un type programme représentant une liste d'instructions donc un set d'instructions.*)

type programm = Programm of (instruction) list;;

(* fonction qui prend en paramètres une liste d'instructions et nos registres*)

let rec execution (Programm programme) registres = 
  match programme with 
    | [] -> []
    | (instruction)::t -> begin match instruction with
                                  | Mono1S_Inc(param(t,n)) -> inc registres n
				  | Mono1S_Dec(param(t,n)) -> dec registres n
                                  | Duo1S(param(t,n,m)) -> (*TODO copy*) 
                                  | Mono2S(param(exit)) -> (*TODO lancer jump qui retourn n un entier. Revenir a la position n dans la liste*)
                                  | Duo2S(instruction) -> (*lancer jumpM qui retourne un entier representant le saut à la ligne d'execution, aller a cette entier dans la liste*)

                                   
;;
