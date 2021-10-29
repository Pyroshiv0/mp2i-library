(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;
(*[taille] renvoie la longueur d'une liste*)
let rec taille l= match l with
            |[]->0
            |e::q-> taille q +1

(*[nieme l n] :Renvoie le n-ième élément de la liste donnée. Le premier élément (tête de liste) est à la position 0.retourne none si la liste est trop courte et Invalid argument si n<0.si la liste est vide et n=0,renvoie une erreur empty list *)
let rec nieme  l n= if n<0 then failwith "Invalid Argument"
                else if n>(taille l -1) then failwith "None,indice too big for the length"
        else match l with
        |[]-> 0(*cette ligne évite l'affichage du message pour annnoncer que le patern matching est non-exhaustif.si la liste est vide,il est impossible d'avoir n-1 inférieur à 0*)
        |e::q->if n=0 then e
                else nieme q (n-1)
