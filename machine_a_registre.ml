(*Magine à registre Axel Verneuil, Valentin Guy-Deroubaix*)


type label = Label of int;; (*name of the nth register*)

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

type ligne = Ligne of int ;; (*ligne number*)

type param = LabelParam of {label : label} 
            | LabelCouple of {label1 : label;label2 : label}
            | Exitparam of {ligne : ligne}
            | LabelExCouple of {label : label; ligneN :ligne}
;;

type instruction = Mono1S of (int Register.t-> label -> ligne->int Register.t*ligne) (*un parametre une sortie*)
                  | Duo1S of (int Register.t->label->label->ligne->int Register.t*ligne) (*deux parametres une sortie*)
                  | Mono2S of (ligne->ligne) (*un parametre deux sorties*)
                  | Duo2S of (int Register.t->label->ligne->ligne->ligne) (*deux parametres deux sorties*)
;;


let inc register n (Ligne ligne) = ((Register.update n (fun e -> match e with 
                                                    | None -> None
                                                    | Some e -> (Some (e+1))) register),(Ligne (ligne + 1)))                    
;;                                                    

let dec registers n (Ligne ligne) = if (Register.find n registers) > 0 
                        then 
                            ((Register.update n (fun e -> match e with                          
                                                    | None -> None
                                                    | Some e -> (Some (e-1))) registers), Ligne (ligne+1)) 
                        else (registers, Ligne (ligne+1))                            
;;                        
let clear registers n (Ligne ligne) = ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some 0)) registers), Ligne (ligne+1))                    
;;                                                    

let jump (Ligne n) = (Ligne n) ;;

let jumpM register m (Ligne n) (Ligne ligne) = if (Register.find m register) = 0 then (Ligne n) else Ligne (ligne+1) 
;;


let compare_line (n :ligne) (m :ligne) = 
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

module InstructionOrdonnee = struct
  type t = ligne;;
  let compare = compare_line;;
end

module Programm = Map.Make(InstructionOrdonnee)

let afficher_registre registre =
  Register.iter (fun key a -> print_int a) registre
;;

let execution programm register = 
  let arret ligne programm = not (Programm.mem ligne programm)
  in
  let rec execution_aux (Ligne ligne) programme registre fin =
    if fin 
      then 
        registre
      else
        match (Programm.find (Ligne ligne) programm) with 
            | (Mono1S instruction,LabelParam param) -> let (regis,nextligne) = (instruction registre param.label (Ligne ligne)) in
                                                      let fin = arret nextligne programme in 
                                                      execution_aux nextligne programm regis fin    
            | (Duo1S instruction,LabelCouple param) -> let (regis,nextligne) = (instruction registre param.label1 param.label2 (Ligne ligne)) in
                                                      let fin = arret nextligne programme in 
                                                      execution_aux nextligne programm registre fin
            | (Mono2S instruction,Exitparam param) -> let fin = arret (Ligne ligne) programm in 
                                                      execution_aux (instruction param.ligne) programm registre fin
            | (Duo2S instruction,LabelExCouple param) -> let fin = arret (Ligne ligne) programm in
                                                        execution_aux (instruction registre param.label param.ligneN (Ligne ligne)) programm registre fin
            | (_) -> failwith("crash")  
  in execution_aux (Ligne 1) programm register false
;;


let registre = Register.empty;;

let registre = Register.add (Label 1) 3 registre;;
let registre = Register.add (Label 2) 3 registre;;
let registre = Register.add (Label 3) 0 registre;;

let add = Programm.add (Ligne 1) (Duo2S jumpM,LabelExCouple {label = (Label 1);ligneN = (Ligne 5)}) Programm.empty;;

let add = Programm.add (Ligne 2) (Mono1S inc,LabelParam {label = (Label 2)}) add;;

let add = Programm.add (Ligne 3) (Mono1S dec,LabelParam {label = (Label 1)}) add;;

let add = Programm.add (Ligne 4) (Mono2S jump,Exitparam {ligne = (Ligne 1)}) add;; (*addition*)


let regsitre = execution add registre;;
