#use "list.ml"
type 'a option = None|Some of 'a;;


type ('a,'b ) table_hachage={hache: 'a -> int ;
                             donnees: ('a * 'b) list ;
                             largueur: int};;
let create h w = let dic= {hache = h; donnees= [] ;largueur= w} in
                dic;;
let rec recherche t k= let rech =match t with
                                |[]-> false
                                |(k1,v)::q-> if k1=k then true
                                            else recherche q k in
                        
                    rech t.donnees k;;
            