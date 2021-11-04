(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;
(*[taille] renvoie la longueur d'une liste*)
let rec taille l= match l with
            |[]->0
            |e::q-> taille q +1 ;;

(*[nieme l n] :Renvoie le n-ième élément de la liste donnée. Le premier élément (tête de liste) est à la position 0.retourne none si la liste est trop courte et Invalid argument si n<0.si la liste est vide et n=0,renvoie une erreur empty list *)
let rec nieme  l n= if n<0 then failwith "Invalid Argument"
                else if n>(taille l -1) then failwith "None,indice too big for the length"
        else match l with
        |[]-> 0(*cette ligne évite l'affichage du message pour annnoncer que le patern matching est non-exhaustif.si la liste est vide,il est impossible d'avoir n-1 inférieur à 0*)
        |e::q->if n=0 then e
                else nieme q (n-1) 
         ;;  
(*[premier l]renvoie le premier élément de l*)
let premier l=nieme l 0 ;;         
(*[recherche l el] renvoie true si el est dans l sinon false*)
let rec recherche l el= match l with
        |[]->false
        |e::q-> if e=el then true
                else recherche q el
         ;;
(*[concat l l2] concatène deux listes l et l2*)
let rec concat l l2= match l with
        |[]->l2
        |e::q-> e::concat q l2;;


(*[inverse l] retourne une liste en utilisant un accumulateur*)
let inverse l=let rec invers l acc =match l with
                    |[]->acc
                    |e::q -> invers q (e::acc)  in
                invers l []  ;;
                (* utilise l'application partielle des fonctions en ocaml pour simplifier l'usage d'invers *) 
       
       
(*[croissant l] vérifie si les indices d'une fonction sont dans l'ordre croissant*)       
let rec croissant = function
            |[]->true
            |[e]->true
            |e::e2::q->if e2 <e then false
                       else croissant (e2::q )
            ;;
  (*[maximum l] donne le maximum de l*)          
let rec maximum  l =match  l with 
        |[]-> min_int
        |e::q-> max e (maximum q) ;;
(*[minimum l] donne le minimum de l*)          
let rec minimum  l =match  l with 
        |[]-> max_int
        |e::q-> min e (minimum q) ;;
        
(*[minmax l] tenvoie un couple d'entier (minimum, maximum) d'une liste l sans utiliser minimum et maximum même si le principe reste le même:c'est de l'entrainement*)
let rec minmax l= match l with
        |[]-> max_int,min_int
        |[e]-> e,e
        |e::f::q->  let mini,maxi= minmax q in
                    if e> f then ( min f mini),(max e maxi)
                    else ( min e mini),(max f maxi )
(*[pairs n] renvoie une liste d'entiers pairs allant de 2n à 0*)
let rec pairs n =if n <0 then []
                 else (2*n)::pairs(n-1);;
                
(*[splitp l*]coupe une liste en deux listes de même taille,à 1 près.les éléments d'indices pairs vont à gauche et ceux impairs à droite*)
let rec splitp =function
        |[]-> [],[]
        |[e]->[e],[]
        |e::f::q->let q1,q2=splitp q in
                      e::q1,f::q2;;
(*[ajout n l l2 ] ajoute les n premiers éléments de l à l2 et retourne l2*)
let rec ajout n l l2 = if  n < 0 then failwith "Invalid n argument"
                       else if n=0 then l2
                            else match l with
                                 |[]->failwith "there is less then n elements in l "
                                 |e::q -> e::(ajout (n-1) q l2) ;;
(*[supr n l supprime les n premiers éléments de l*)
let rec supr n l  = if  n < 0 then failwith "Invalid n argument"
                       else if n=0 then l
                            else match l with
                                 |[]->failwith "there is less then n elements in l "
                                 |e::q -> supr (n-1) q ;;
(*[split l coupe une liste en deux listes de même taille à 1 près.Les éléments inférieurs ou égaux à (n(+1 si n impair))/2 sont à gauche*)
let split l= let rec spli l l2= let nl= ref (taille l) in
                                if !nl mod 2 <>0 then incr nl ;
                                
                                let q1 =ajout (!nl/2) l l2 in 
                                let q2= supr (!nl/2) l in
                                q1,q2 in
             spli l [];;
(*[fusion l1 l2 fusionne deux listes triées en une liste triée*)
let rec fusion l1 l2= if taille (l1) =0 then l2
                      else if taille(l2)=0 then l1
        else match l1 with
        |[]->l2
        |e::q->if taille(l2)<>0 then if e <= premier l2 then e::(fusion q l2)
                                         else (premier l2)::fusion (e::q) (supr 1 l2)
            else []    
(*[tri_ fusion l] effectue le tri fusion de l1*)
let  rec tri_fusion l1= if taille(l1)<= 1 then l1
                        else begin 
                             let q1,q2 = split l1 in
                             fusion (tri_fusion(q1)) (tri_fusion(q2)) 
                             end
