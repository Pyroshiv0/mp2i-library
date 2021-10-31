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
        
        
let rec minmax l= match l with
        |[]->(min_int,max_int)
        |[e]->(e,e)
        |[e,f]->if e<f then (e,f)
                    else (f,e)
        |e::f::q-> if e> f then (( min f  (minmax q),(max e (minmax q))))
                   else (( min e  (minmax q),(max f (minmax q))))
