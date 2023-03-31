(*Magine à registre Axel Verneuil, Valentin Guy-Deroubaix*)


type label = Label of int;; (*name of the nth register*)

type ligne = Ligne of int ;; (*ligne number*)


(*création d'un type registre, un dictionnaire ayant pour clé un label et pour contenue un entier, les registres représente la mémoire*)
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

(*Le type param et le type instruction sont les deux types pricipaux qui vont régir comment fonctionne l'execution d'une programme *)

type param = LabelParam of {label : label} 
            | LabelCouple of {label1 : label;label2 : label}
            | LigneParam of {ligne : ligne}
            | LabelLigneCouple of {label : label; ligneN :ligne}
;;

type instruction = Mono1S of (int Register.t-> label -> ligne->int Register.t*ligne) (*un parametre une sortie*)
                  | Duo1S of (int Register.t->label->label->ligne->int Register.t*ligne) (*deux parametres une sortie*)
                  | Mono2S of (ligne->ligne) (*un parametre deux sorties*)
                  | Duo2S of (int Register.t->label->ligne->ligne->ligne) (*deux parametres deux sorties*)
;;

(*------------------------------------------------------------------------------------------------*)
(*Création du sets d'instructions de L'URM
  On rajoute des prints dans les inctructions pour voir comment évolue les registres au fil de l'execution *)
let inc registers n (Ligne ligne)  = (print_string("On lance l'instruction add:");
				                             print_newline();
				                             ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some (e+1))) registers),(Ligne(ligne+1))))                            
;;

let dec registers n (Ligne ligne) = (print_string("On lance l'instruction dec:");
				                            print_newline();
                                    if (Register.find n registers) > 0 then 
                            		       ((Register.update n (fun e -> match e with
                                                    			| None -> None
                                                    			| Some e -> (Some (e-1))) registers),(Ligne(ligne+1)))
                        	          else (registers, (Ligne(ligne+1))))
;;

let clear registers n (Ligne line) =  (print_string("On lance l'instruction clear:");
				                              print_newline();
                                      ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some 0)) registers),(Ligne(line+1))))                            
;;                                            

let copy registers m n (Ligne ligne) =  (print_string("On lance l'instruction copy:");
                                        print_newline();
                                        ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some (Register.find m registers))) registers), Ligne (ligne+1)))
;;

let jump (Ligne n) = (print_string("On lance l'instruction jump:");
		                 print_newline();
                     (Ligne n)) 
;;

let jumpM register m (Ligne n) (Ligne ligne) = (print_string("On lance l'instruction jumpM:");
				  	     print_newline();
                    			     if (Register.find m register) = 0 then (Ligne n) else (Ligne(ligne+1)))
;;

(*Fonction pour afficher le dictionnaire de registres voulus*)
let display_registers registers = Register.iter (fun key a -> print_int a;print_newline()) registers;;


(*----------------------------------------------------------------------------------------------------------------------------------*)
(*Création de la première méthode d'execution avec les (instruction*param) list *)

let rec parcours liste (Ligne tmp) =
	match liste with
	|[] -> []
	|x::s -> begin match Ligne tmp with
		    |Ligne 1 -> [x]@s
		    |Ligne _ -> parcours s (Ligne (tmp-1))
			
	      end
;;


let  execution1 programm registers = 
let rec execution' (Ligne ligne) programm registers tmp =
	let test = parcours programm (Ligne ligne) in
		if test  == []  then
			registers
		else
			(*Regle prend la valeur de l'instruction que l'on doit traiter*)
			let regle = List.hd test in	
  			match regle with
			|(Mono1S instruction, LabelParam param) -> let (update_registers,(Ligne ligne)) = (instruction registers param.label (Ligne ligne)) in	
									                               (print_string("Tour"^" "^string_of_int(tmp)^":");
									                               print_newline();
									                               display_registers update_registers; 
								   	                             execution' (Ligne ligne) programm update_registers (tmp+1))

			|(Duo1S instruction, LabelCouple param) -> let (update_registers,(Ligne ligne)) = (instruction registers param.label1 param.label2 (Ligne ligne)) in				 	
                                                 (print_string("Tour"^" "^string_of_int(tmp)^":");
                                                 print_newline();
                                                 display_registers update_registers; 
                                                 execution' (Ligne ligne)  programm update_registers (tmp+1))

			|(Mono2S instruction, LigneParam param) -> (print_string("Tour"^" "^string_of_int(tmp)^":");
								                                 print_newline();
								                                 display_registers registers;
                                                 execution' (instruction param.ligne) programm registers (tmp+1))

			|(Duo2S instruction, LabelLigneCouple param) -> (print_string("Tour"^" "^string_of_int(tmp)^":");
                                                      print_newline();
                                                      display_registers registers; 
                                                      execution' (instruction registers param.label param.ligneN (Ligne ligne)) programm registers (tmp+1))

			|(_) -> failwith("error")
in execution' (Ligne 1) programm registers 1
;;

(*---------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*Initialisation des objets nécéssaire pour la création de la méthode 2 d'excution d'un programme, 
  ici un programme est un dictionnaire ayant pour clé des lignes et de contenue un couple (instruction,param)*)
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

let execution2 programm register = 
  let arret ligne programm = not (Programm.mem ligne programm)
  in
  let rec execution_aux (Ligne ligne) programme registre fin tmp =
    if fin 
      then 
        registre
      else
        match (Programm.find (Ligne ligne) programm) with 
            | (Mono1S instruction,LabelParam param) -> let (regis,nextligne) = (instruction registre param.label (Ligne ligne)) in
                                                       let fin = arret nextligne programme in 
                                                       (print_string("Tour"^" "^string_of_int(tmp)^":");
									                                     print_newline();
									                                     display_registers regis;
                                                       execution_aux nextligne programm regis fin (tmp+1))  

            | (Duo1S instruction,LabelCouple param) -> let (regis,nextligne) = (instruction registre param.label1 param.label2 (Ligne ligne)) in
                                                       let fin = arret nextligne programme in
                                                       (print_string("Tour"^" "^string_of_int(tmp)^":");
                                                       print_newline();
                                                       display_registers regis;  
                                                       execution_aux nextligne programm regis fin (tmp+1))

            | (Mono2S instruction,LigneParam param) ->  let fin = arret (Ligne ligne) programm in 
                                                        (print_string("Tour"^" "^string_of_int(tmp)^":");
                                                        print_newline();
                                                        display_registers registre;        
                                                        execution_aux (instruction param.ligne) programm registre fin (tmp+1))

            | (Duo2S instruction,LabelLigneCouple param) -> let fin = arret (Ligne ligne) programm in
                                                            (print_string("Tour"^" "^string_of_int(tmp)^":");
                                                            print_newline();
                                                            display_registers registre; 
                                                            execution_aux (instruction registre param.label param.ligneN (Ligne ligne)) programm registre fin (tmp+1)) 
            | (_) -> failwith("crash")  
  in execution_aux (Ligne 1) programm register false 1
;;

(*fonction pour initialiser les registres dont on a besoin à 0*)
let initRegistre nbRegistre = 
  let rec init nb registre = 
  if nb <= nbRegistre && nbRegistre > 0 
    then
      init (nb+1) (Register.add (Label nb) 0 registre)
    else 
      if nb = 0  
        then 
          registre
        else 
          if nbRegistre = 0
            then
              Register.empty
            else
              failwith "Il faut un nombre de registre positif"    
  in init 1 Register.empty
;;


(*Exemple d'utilisation création d'opération arithmétique*)
(*--------------------------------------------------------------------------------------------------------------------------------------------------*)
(*Méthode 1 d'execution*)

(*On remplit les registres à la main, c'est plus confortable*)
let registre = Register.empty;;
let registre = Register.add (Label 1) 5 registre;;
let registre = Register.add (Label 2) 1 registre;;
let registre = Register.add (Label 4) 0 registre;;
let registre = Register.add (Label 5) 0 registre;;


let addition = [(Duo2S jumpM, LabelLigneCouple {label = (Label 2); ligneN = (Ligne 5)});
                (Mono1S inc, LabelParam {label = (Label 1)});
                (Mono1S dec, LabelParam {label = (Label 2)}); 
                (Mono2S jump, LigneParam {ligne = (Ligne 1)})]
;;

let multiplication = [(Duo1S copy,LabelCouple {label1 = (Label 1); label2 = (Label 4)});
                      (Duo2S jumpM, LabelLigneCouple {label = (Label 4); ligneN = (Ligne 6)});
                      (Mono1S inc, LabelParam {label = (Label 5)});
                      (Mono1S dec, LabelParam {label = (Label 4)});
                      (Mono2S jump, LigneParam {ligne = (Ligne 2)});
                      (Mono1S dec, LabelParam {label = (Label 2)});
                      (Duo2S jumpM, LabelLigneCouple {label = (Label 2); ligneN = (Ligne 9)});
                      (Mono2S jump, LigneParam {ligne = (Ligne 1)})]
;;

let soustraction = [(Duo2S jumpM, LabelLigneCouple {label = (Label 2); ligneN = (Ligne 5)});
                    (Mono1S dec, LabelParam {label = (Label 1)});
                    (Mono1S dec, LabelParam {label = (Label 2)});
                    (Mono2S jump, LigneParam {ligne = (Ligne 1)})]
;;

let division = [(Duo1S copy,LabelCouple {label1 = (Label 2); label2 = (Label 4)});
                (Duo2S jumpM, LabelLigneCouple {label = (Label 4); ligneN = (Ligne 6)});
                (Mono1S dec, LabelParam {label = (Label 1)});
                (Mono1S dec, LabelParam {label = (Label 4)});
                (Mono2S jump, LigneParam {ligne = (Ligne 2)});
                (Mono1S inc, LabelParam {label = (Label 5)});
                (Duo2S jumpM, LabelLigneCouple {label = (Label 1); ligneN = (Ligne 9)});
                (Mono2S jump, LigneParam {ligne = (Ligne 1)})]
;;

(*Exemple de test pour faire un simple calcul avec l'approche1*)
(*let test = execution1 division registre;;*)

(*-------------------------------------------------------------------------------------------------------------------------------*)
(*Méthode 2 d'execution*)

let registre2 = Register.empty;;

let registre2 = Register.add (Label 1) 3 registre2;;
let registre2 = Register.add (Label 2) 3 registre2;;
let registre2 = Register.add (Label 3) 0 registre2;;

let addition = Programm.add (Ligne 1) (Duo2S jumpM,LabelLigneCouple {label = (Label 1);ligneN = (Ligne 5)}) Programm.empty;;

let addition = Programm.add (Ligne 2) (Mono1S inc,LabelParam {label = (Label 2)}) addition;;

let addition = Programm.add (Ligne 3) (Mono1S dec,LabelParam {label = (Label 1)}) addition;;

let addition = Programm.add (Ligne 4) (Mono2S jump,LigneParam {ligne = (Ligne 1)}) addition;; 

(* Un autre exemple pour faire un calcul avec l'approche 2*)
(*let registre = execution2 addition registre2;; *)
