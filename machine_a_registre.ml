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
type line = Line of int ;; (*ligne number*)

(*------------------------------------------------------------------------------------------------*)
(*Création du sets d'instructions de L'URM*)
let inc registers n (Line line)  = ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some (e+1))) registers),(Line(line+1)))                             
;;

let dec registers n (Line line) = if (Register.find n registers) > 0 then 
                            		((Register.update n (fun e -> match e with
                                                    			| None -> None
                                                    			| Some e -> (Some (e-1))) registers),(Line(line+1)))
                        	  else (registers, (Line(line+1)))
;;

let clear registers n (Line line) = ((Register.update n (fun e -> match e with
                                                    | None -> None
                                                    | Some e -> (Some 0)) registers),(Line(line+1)))                            
;;

let jump (Line n) = (Line n) ;;

let jumpM register m (Line n) (Line line) = if (Register.find m register) = 0 then (Line n) else (Line(line+1)) ;;

(*TODO: faire l'instruction copy*)

(*------------------------------------------------------------------------------------------------*)
(*Création du type param pour les différents paramètres. On créer également un type instuction car les instructions n'ont pas le même nombre de sortie*)

type param = LabelParam of {label: label; line: line} (*Pour Inc et Dec et clear*)
            | LabelCouple of {label1: label; label2: label; line: line} (*Pour copy*) 
            | ExitParam of {line: line} (*Pour jump*)
            | LabelExCouple of {label: label; lineN: line; line: line} (*Pour jumpM*) 
;;

type instruction = Mono1S of (int Register.t -> label -> line -> int Register.t*line) (*type est une fonction qui prend en entrée un le dico_registes, un label et une ligne. Elle renvoi un couple(dico_registres update,line)*)
                  | Duo1S of (int Register.t -> label -> label -> line -> int Register.t*line) (*type avec dico_registre avec deux label et une ligne et qui renvoi le couple avec le dico_update et la line. 2 registre en parametre*)
                  | Mono2S of (line -> line) (*un parametre deux sorties. renvoi la ligne pour le saut*)
                  | Duo2S of (int Register.t -> label-> line -> line -> line) (*deux parametres deux sorties. renvoi soit la ligne pour sauter sur une instruction précèdente soit un ligne suivante exterieur pour stopper le prog*)
;;
(*----------------------------------------------------------------------------------------------*)
(* On passe à la création de nos opérations arithmétiques. Pour cela, on créer un type programme représentant une liste d'instructions donc un set d'instructions.*)



let rec parcours liste (Line tmp) =
	match liste with
	|[] -> []
	|x::s -> begin match Line tmp with
		    |Line 1 -> [x]@s
		    |Line _ -> parcours s (Line (tmp-1))
			
	      end
;;

let display_registers registers = Register.iter (fun key a -> print_int a) registers;;

(* fonction qui prend en paramètres une liste d'instructions et nos registres*)
(* Manière laborieuse: je force la variable programm a etre une liste de (instruction,param)*) 
let  execution programm registers = 
let rec execution' (Line line) programm registers =
	let test = parcours programm (Line line) in
		if test  == []  then
			registers
		else
			(*regle prend la valeur de l'instruction que l'on doit traiter*)
			let regle = List.hd test in	
  			match regle with
			|(Mono1S instruction, LabelParam param) -> let (update_registers,(Line line)) = (instruction registers param.label param.line) in
									print_string("mono1S"); 
								   	execution' (Line line) programm update_registers

			|(Duo1S instruction, LabelCouple param) -> let (update_registers,(Line line)) = (instruction registers param.label1 param.label2 param.line) in
								 	execution' (Line line)  programm update_registers

			|(Mono2S instruction, ExitParam param) -> print_string("mono2s");execution' (instruction param.line) programm registers

			|(Duo2S instruction, LabelExCouple param) -> execution' (instruction registers param.label param.lineN param.line) programm registers

			|(_) -> failwith("error")

in execution' (Line 1) programm registers

;;
(*-------------------------------------------------------------------------------------------------------*)
(*Opération arithmétique*)

(*let registers = List.fold_left (fun m (k,v) -> Register.add k v m) Register.empty [((Label 1), 4);((Label 2), 5)];;*)
let registre = Register.empty;;
let registre = Register.add (Label 1) 4 registre;;
let registre = Register.add (Label 2) 3 registre;;
let registre = Register.add (Label 4) 0 registre;;

let addition_Instruction = [(Duo2S jumpM, LabelExCouple {label = (Label 2); lineN = (Line 5); line = (Line 1)});
				      (Mono1S inc, LabelParam {label = (Label 1); line = (Line 2)});
				      (Mono1S dec, LabelParam {label = (Label 2); line = (Line 3)}); 
				      (Mono2S jump, ExitParam {line = (Line 1)})]
;;

(*let test = execution addition_Instruction registre;;*)


