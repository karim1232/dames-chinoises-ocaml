(* Projet Dames Chinoises *)

type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;



type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

(*type direction NE | NO | E | O | SE | SO;;*)

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);; 

(* Q1 *) 
  
(*1. le triangle sud *)
(*2. le triangle nord *)
(*3. le triangle nord west *)
(*4. sommet nord *)
(*5. sommet gauche de triangle sud *)
(*6. tous les points sauf les triangles nord, sud, nord est, nord west *)

(* Q2 *)

(*  j >= -dim && j <= dim && k >= -dim && k <= dim *)

(* Vérifie si une case est dans le losange défini par la dimension *)
let est_dans_losange(c: case)(dim: dimension): bool = 
  let (i, j, k) = c in  
  j >= -dim && j <= dim && k >= -dim && k <= dim;;

assert(est_dans_losange (4,0,-6) 3 = false) ;;
assert(est_dans_losange (0,0,0) 3 = true) ;;

(* Q3 *)

(* Vérifie si une case est dans l'étoile définie par la dimension *)
let est_dans_etoile(c: case)(dim: dimension): bool =
  let (i, j, k) = c in
  (i >= -dim && j >= -dim && k >= -dim) || (i <= dim && j <= dim && k <= dim);; 

(* Q4 *)

(* Effectue une rotation sur une case selon un certain nombre de rotations *)
let tourner_case(m:int)(c:case):case =
  let (i, j, k) = c in
  match (m mod 6) with 
  | 0 -> (i, j, k)
  | 1 -> (-k, -i, -j)
  | 2 -> (j, k, i)
  | 3 -> (-i, -j, -k)
  | 4 -> (k, i, j)
  | 5 -> (-j, -k, -i)
  | _ -> (i, j, k);;

assert(tourner_case 9 (-6,3,3) = (6,-3,-3)) ;;

(* Q5 *)

(* Effectue une translation sur une case selon un vecteur donné *)
let translate(c:case)(v:vecteur):case =
  let (i, j, k) = c and (v1, v2, v3) = v in
  (i + v1, j + v2, k + v3);;

(* Q6 *)

(* Calcule la différence entre deux cases pour obtenir un vecteur *)
let diff_case(c1:case)(c2:case):vecteur =
  let (i1, j1, k1) = c1 and (i2, j2, k2) = c2 in
  (i1 - i2, j1 - j2, k1 - k2);;

(* Q7 *)

(* Vérifie si deux cases sont voisines *)
let sont_cases_voisines(c1:case)(c2:case):bool =
  let (i1, j1, k1) = c1 and (i2, j2, k2) = c2 in
  if (i1 = i2) && (j1 + 1 = j2 || j2 + 1 = j1) && (k1 + 1 = k2 || k2 + 1 = k1) then true
  else if (j1 = j2) && (i1 + 1 = i2 || i2 + 1 = i1) && (k1 + 1 = k2 || k2 + 1 = k1) then true
  else if (k1 = k2) && (j1 + 1 = j2 || j2 + 1 = j1) && (i1 + 1 = i2 || i2 + 1 = i1) then true
  else false;;

assert(sont_cases_voisines (0,1,-1) (0,2,-2) = true ) ;;
assert(sont_cases_voisines (0,1,-1) (0,1,-2) = false ) ;; 

(* Q8 *)

(* Calcule le pivot entre deux cases si elles sont alignées *)
let calcul_pivot(c1:case)(c2:case):case option =
  let (i1, j1, k1) = c1 and (i2, j2, k2) = c2 and (v1, v2, v3) = (diff_case c1 c2) in
  if ((v1 mod 2 = 1) || (v2 mod 2 = 1) || (v3 mod 2 = 1)) || (i1 != i2 && j1 != j2 && k1 != k2) 
  then 
    None 
  else 
    let (i3, j3, k3) = translate c1 c2 in 
    Some (i3 / 2, j3 / 2, k3 / 2) ;;

assert(calcul_pivot (-4,3,1) (0,-1,1) = Some (-2,1,1)) ;;

(* Q9 *)

(* Calcule le vecteur et la distance entre deux cases alignées *)
let vec_et_dist(c1:case)(c2:case):vecteur*int =
  let (i1, j1, k1) = c1 and (i2, j2, k2) = c2 in
  if (i1 != i2 && j1 != j2 && k1 != k2) then failwith "les vecteurs ne sont pas alignées"
  else let (v1,v2,v3)=(diff_case c1 c2) in
    if v1>0 then ((-v1/v1,-v2/v1,-v3/v1),v1) 
    else if v2>0 
    then
      ((-v1/v2,-v2/v2,-v3/v2),v2) 
    else
      ((-v1/v3,-v2/v3,-v3/v3),v3) ;;

assert(vec_et_dist (6,-3,-3) (4,-3,-1)  = ((-1,0,1),2)) ;;
assert(vec_et_dist (-1,1,0) (-4,1,3) = ((-1,0,1),3));;

(* Q10 *)

(* Effectue une rotation circulaire sur une liste *)
let rec tourner_liste(l:'a list):'a list =
  match l with
  |[] -> []
  |[e] -> [e] 
  |e1::e2::suite->e2::(tourner_liste (e1::suite));;

assert(tourner_liste [Vert;Jaune;Rouge]=[Jaune;Rouge;Vert]);;
assert(tourner_liste [4;2;65;87;9;3]=[2;65;87;9;3;4]);;
assert(tourner_liste [3]=[3]);;

(* Retourne le dernier élément d'une liste *)
let rec der_liste(l:'a list):'a=
  match l with
  |[] -> failwith "Liste est vide"
  |[e] -> e
  |e1::suite -> der_liste suite;;

assert(der_liste [Vert; Jaune; Rouge] = Rouge);;
assert(der_liste [4;2;65;87;9;3]=3);;
assert(der_liste [2]=2);; 

(* Q11 *)

(* Remplit une séquence de cases à partir de la case (i,j,k) *)
let rec remplir_segment (n:int) (c:case) : case list =
  if (est_case c)<>true then failwith "ce n'est pas un case"
  else
    match n, c with
    | 1, (i,j,k) -> [(i,j,k)] (* Cas de base : si n = 1, retourne simplement la case *)
    | n, (i,j,k) -> (i,j,k)::(remplir_segment (n-1) (i,j+1,k-1)) ;; (* Cas récursif : ajoute la case courante et appelle la fonction récursivement *)

assert (remplir_segment 3 (-4,1,3) = [(-4,1,3); (-4,2,2); (-4,3,1)]);
assert (remplir_segment 3 (0,0,0) = [(0,0,0); (0,1,-1); (0,2,-2)]);
assert (remplir_segment 1 (0,0,0) = [(0,0,0)]);;

(* Q12 *)

(* Remplit un triangle bas à partir de la case (i,j,k) *)
let rec remplir_triangle_bas (n:int) (c:case) : case list = 
  match n, c with
  | 1, (i,j,k) -> [(i,j,k)] (* Cas de base : si n = 1, retourne simplement la case *)
  | n, (i,j,k) -> (remplir_segment n (i,j,k))@(remplir_triangle_bas (n-1) (i-1,j+1,k));; (* Cas récursif : ajoute la séquence de cases du segment actuel et appelle la fonction récursivement *)

assert (remplir_triangle_bas 3 (-4,1,3) = [(-4,1,3); (-4,2,2); (-4,3,1); (-5,2,3); (-5,3,2); (-6,3,3)]);
assert (remplir_triangle_bas 3 (4,-3,-1) = [(4,-3,-1); (4,-2,-2); (4,-1,-3); (3,-2,-1); (3,-1,-2); (2,-1,-1)]);
assert (remplir_triangle_bas 1 (0,0,0) = [(0,0,0)]);;

(* Q13 *)

(* Remplit un triangle haut à partir de la case (i,j,k) *)
let rec remplir_triangle_haut (n:int) (c:case) : case list = 
  match n, c with
  | 1, (i,j,k) -> [(i,j,k)] (* Cas de base : si n = 1, retourne simplement la case *)
  | n, (i,j,k) -> (remplir_segment n (i,j,k))@(remplir_triangle_haut (n-1) (i+1,j,k-1));; (* Cas récursif : ajoute la séquence de cases du segment actuel et appelle la fonction récursivement *)

assert (remplir_triangle_haut 3 (-3,4,-1) = [(-3,4,-1); (-3,5,-2); (-3,6,-3); (-2,4,-2); (-2,5,-3); (-1,4,-3)]);
assert (remplir_triangle_haut 3 (4,-3,-1) = [(4,-3,-1); (4,-2,-2); (4,-1,-3); (5,-3,-2); (5,-2,-3); (6,-3,-3)]);
assert (remplir_triangle_haut 1 (0,0,0) = [(0,0,0)]);;

(* Q14 *)

(* Associe une couleur à chaque case d'une liste de cases *)
let rec colorie (coul:couleur) (lc:case list) : case_coloree list = 
  match coul, lc with
  | coul, [] -> []
  | coul, [c] -> [(c,coul)]
  | coul, c1::c2 -> (c1,coul)::(colorie coul c2);;

assert (colorie Rouge [(-4,1,3);(-4,2,2);(-4,3,1)] = [((-4,1,3),Rouge);((-4,2,2),Rouge);((-4,3,1),Rouge)]);
assert (colorie Vert [(-3,4,-1);(-3,5,-2);(-3,6,-3);(-2,4,-2);(-2,5,-3);(-1,4,-3)] = [((-3,4,-1),Vert);((-3,5,-2),Vert);((-3,6,-3),Vert); ((-2,4,-2),Vert);((-2,5,-3),Vert);((-1,4,-3),Vert)]);;

(* Q15 *)

(* Tourne une liste de cases colorées selon un motif *)
let rec tourner_lc (lc:case_coloree list) (m:int) : case_coloree list =
  match lc with
  | [] -> []
  | [(e,coul)] -> [((tourner_case m e),coul)]
  | (e1,coul)::suite -> ((tourner_case m e1),coul)::(tourner_lc suite m);;

(* Tourne une configuration *)
let tourner_config (conf:configuration) : configuration =
  let (lc, lcoul, d) = conf in 
  let l = List.length lcoul in 
  ((tourner_lc lc (6/l)), (tourner_liste lcoul), d);;

(* Définition des configuration pour les Tests *)
let conf1 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
              ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
              ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); 
              ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
              ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
              ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)],[Vert;Rouge;Jaune],3);;
let conf2 = ([((-6, 3, 3), Rouge); ((-5, 2, 3), Rouge); ((-4, 1, 3), Rouge);
              ((-5, 3, 2), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);
              ((1, 3, -4), Vert); ((2, 2, -4), Vert); ((3, 1, -4), Vert);
              ((3, 2, -5), Vert); ((3, 3, -6), Vert); ((1, -4, 3), Jaune); 
              ((2, -5, 3), Jaune); ((3, -6, 3), Jaune); ((2, -4, 2), Jaune); 
              ((3, -5, 2), Jaune); ((2, 3, -5), Vert); ((3, -4, 1), Jaune)], [Rouge; Jaune; Vert], 3);;
(* Vérifie que la fonction tourner_config effectue correctement la rotation *)
assert (tourner_config conf1
        = ([((-6, 3, 3), Rouge); ((-5, 2, 3), Rouge); ((-4, 1, 3), Rouge);
            ((-5, 3, 2), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);
            ((1, 3, -4), Vert); ((2, 2, -4), Vert); ((3, 1, -4), Vert);
            ((2, 3, -5), Vert); ((3, 2, -5), Vert); ((3, 3, -6), Vert);
            ((1, -4, 3), Jaune); ((2, -5, 3), Jaune); ((3, -6, 3), Jaune);
            ((2, -4, 2), Jaune); ((3, -5, 2), Jaune); ((3, -4, 1), Jaune)], [Rouge; Jaune; Vert], 3));;

assert (tourner_config conf2
        =([((3, 3, -6), Rouge); ((2, 3, -5), Rouge); ((1, 3, -4), Rouge);
           ((3, 2, -5), Rouge); ((2, 2, -4), Rouge); ((3, 1, -4), Rouge);
           ((3, -4, 1), Vert); ((2, -4, 2), Vert); ((1, -4, 3), Vert);
           ((2, -5, 3), Vert); ((3, -6, 3), Vert); ((-4, 3, 1), Jaune);
           ((-5, 3, 2), Jaune); ((-6, 3, 3), Jaune); ((-4, 2, 2), Jaune);
           ((-5, 2, 3), Jaune); ((3, -5, 2), Vert); ((-4, 1, 3), Jaune)],
          [Jaune; Vert; Rouge], 3));;

(* Q16 *)
(* Fonction pour tourner une liste de cases *)
let rec tourner_lcase (lc:case list) (m:int) : case list =
  match lc with
  | [] -> []
  | [e] -> [(tourner_case m e)]
  | e1::suite -> (tourner_case m e1) :: (tourner_lcase suite m)

(* Fonction pour remplir la configuration initiale *)
let remplir_init (l:couleur list) (d:dimension):configuration =
  (* Nombre de joueurs *)
  let nb_joueurs = List.length l in
  (* Coordonnées de départ pour le protagoniste *)
  let i = (-2 * d) + ( d - 1) in
  let j = (2 * d) - ((2 * d) - 1) in
  let k = ((2 * d) - d) in
  (* Liste des cases du protagoniste *)
  let l1 = remplir_triangle_bas d (i, j, k) in

  (* Cases coloriées pour le premier joueur *)
  let couleur1 = List.hd l in
  let lc_color1 = colorie couleur1 l1 in

  match nb_joueurs with
  | 1 -> (lc_color1, l, d) (* Cas d'un seul joueur *)
  | 2 ->
    (* Rotation de la liste des couleurs *)
      let color = tourner_liste l in
    (* Rotation des cases pour le deuxième joueur *)
      let l2 = tourner_lcase l1 (6 / nb_joueurs) in
      let lc_color2 = colorie (List.hd color) l2 in
      ((lc_color1 @ lc_color2), l, d)
  | 3 ->
    (* Rotation de la liste des couleurs *)
      let color1 = tourner_liste l in
    (* Rotation des cases pour le deuxième joueur *)
      let l2 = tourner_lcase l1 (6 / nb_joueurs) in
      let lc_color2 = colorie (List.hd color1) l2 in
    (* Rotation de la liste des couleurs *)
      let color2 = tourner_liste color1 in
    (* Rotation des cases pour le troisième joueur *)
      let l3 = tourner_lcase l2 (6 / nb_joueurs) in
      let lc_color3 = colorie (List.hd color2) l3 in
      ((lc_color1 @ lc_color2 @ lc_color3), l, d)
  | 6 ->
    (* Rotation de la liste des couleurs *)
      let color1 = tourner_liste l in
    (* Rotation des cases pour le deuxième joueur *)
      let l2 = tourner_lcase l1 (6 / nb_joueurs) in
      let lc_color2 = colorie (List.hd color1) l2 in
    (* Rotation de la liste des couleurs *)
      let color2 = tourner_liste color1 in
    (* Rotation des cases pour le troisième joueur *)
      let l3 = tourner_lcase l2 (6 / nb_joueurs) in
      let lc_color3 = colorie (List.hd color2) l3 in
    (* Rotation de la liste des couleurs *)
      let color3 = tourner_liste color2 in
    (* Rotation des cases pour le quatrième joueur *)
      let l4 = tourner_lcase l3 (6 / nb_joueurs) in
      let lc_color4 = colorie (List.hd color3) l4 in
    (* Rotation de la liste des couleurs *)
      let color4 = tourner_liste color3 in
    (* Rotation des cases pour le cinquième joueur *)
      let l5 = tourner_lcase l4 (6 / nb_joueurs) in
      let lc_color5 = colorie (List.hd color4) l5 in
    (* Rotation de la liste des couleurs *)
      let color5 = tourner_liste color4 in
    (* Rotation des cases pour le sixième joueur *)
      let l6 = tourner_lcase l5 (6 / nb_joueurs) in
      let lc_color6 = colorie (List.hd color5) l6 in
      ((lc_color1 @ lc_color2 @ lc_color3 @ lc_color4 @ lc_color5 @ lc_color6), l, d)
  | _ -> failwith "Nombre de joueurs incorrects" 

(* Q17 *)

(* Associe une valeur à une clé dans une liste de tuples *)
let rec associe (a:'a) (l:('a * 'b) list) (defaut:'b) : 'b =
  match l with
  | [] -> defaut
  | [(e,coul)] -> if e = a then coul else defaut
  | (e,coul)::suite -> if e = a then coul else (associe a suite defaut);;

(* Retourne la couleur d'une case dans une configuration *)
let quelle_couleur (c:case) (conf:configuration) : couleur =
  let (lc, lcoul, d) = conf in (associe c lc Libre);;

(* Assertion pour tester la fonction quelle_couleur *)
assert (quelle_couleur (-6,3,3) conf1 = Vert);;

assert (quelle_couleur (6,-3,-3) conf1 = Libre);;

(* Fonctions du fichier *)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1);;

(* Tests pour tourner_config avec l'affichage *) 
affiche conf1;;
affiche (tourner_config conf1);; 

let conf3 = ([((0, 0, 0), Jaune)], [Jaune], 2);;
affiche conf3;; 
let conf_vide = ([], [], 2);;
affiche conf_vide;;

(* Le test d'affichage de la fonction remplir_init Q16 *)
(* Définition de la configuration conf_reggae *)
let conf_reggae = ([(0,-1,1), Vert; (0,0,0), Jaune; (0,1,-1), Rouge], [Vert; Jaune; Rouge], 1);;
(* Affichage de conf_reggae *)
affiche conf_reggae;;

(* Définition de la configuration conf4 *)
let conf4 = remplir_init [Vert; Jaune] 3;;
(* Affichage de conf4 *)
affiche conf4;;

(* Définition de la configuration conf5 *)
let conf5 = remplir_init [Jaune; Vert; Rouge] 2;;
(* Affichage de conf5 *)
affiche conf5;;

(* Définition de la configuration conf6 *)
let conf6 = remplir_init [Rouge; Vert; Jaune; Noir; Bleu; Marron] 3;;
(* Affichage de conf6 *)
affiche conf6;;

(* Définition de la configuration conf7 *)
let conf7 = remplir_init [Rouge; Vert; Jaune; Noir; Bleu; Marron] 2;;
(* Affichage de conf7 *)
affiche conf7;;

(* Définition de la configuration conf8 *)
let conf8 = remplir_init [Rouge; Vert] 1;;
(* Affichage de conf8 *)
affiche conf8;;

(* Définition de la configuration conf9 *)
let conf9 = remplir_init [Jaune; Vert; Rouge] 1;;
(* Affichage de conf9 *)
affiche conf9;;

(* Définition de la configuration conf10 *)
let conf10 = ([(0,2,-2), Vert; (-3, 1, 2), Jaune; (-3, 2, 1), Jaune; (-4, 2, 2), Jaune], [Jaune; Vert], 2);;
(* Affichage de conf10 *)
affiche conf10;;

(* Définition de la configuration conf11 *)
let conf11 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-3, 1, 2), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); 
               ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
               ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
               ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)],
              [Vert;Rouge;Jaune],3) ;;
(* Affichage de conf11 *)
affiche conf11;;

(* Définition de la configuration conf12 *)
let conf12=([((0, 0, 0), Jaune); ((-3, 2, 1), Jaune); ((-4, 2, 2), Jaune);((0,2,-2),Vert);((-2,0,0),Vert);((-2,2,0),Vert)], [Vert; Jaune], 2);;
(* Affichage de conf12 *)
affiche conf12;;

(* Définition de la configuration conf13 *)
let conf13=([((0, 0, 0), Jaune); ((-4, 1, 3), Jaune); ((-4, 2, 2), Jaune);((0,2,-2),Vert);((-2,2,0),Vert);((-3,0,3),Vert)], [Vert; Jaune], 3);; 
(* Affichage de conf13 *)
affiche conf13;;

(* Définition de la configuration conf14 *)
let conf14 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 1, 3), Vert); ((-3, 2, 1), Vert); ((-1, 1, 0), Vert); 
               ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
               ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
               ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)],[Vert;Rouge;Jaune],3);;
(* Affichage de conf14 *)
affiche conf14;;

(* Définition de la configuration conf15 *)
let conf15 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); 
               ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
               ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
               ((-2, 0, 2), Jaune); ((1, -2, 1), Jaune); ((1, 3, -4), Jaune)], [Vert;Rouge;Jaune],3);;
(* Affichage de conf15 *)
affiche conf15;;

(* Définition de la configuration conf16 *)
let conf16 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); 
               ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
               ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
               ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)], [Vert;Rouge;Jaune],3);;
(* Affichage de conf16 *)
affiche conf16;;

(* Définition de la configuration conf17 *)
let conf17 = ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
               ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
               ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
               ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-2, 1, 1), Vert)], 
              [Vert; Rouge; Jaune], 3);;
(* Affichage de conf17 *)
affiche conf17;;

affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;

(* Q18 *)

(* Supprime une case d'une liste de cases colorées *)
let rec supprime_dans_lc (lc:case_coloree list) (c:case) : case_coloree list = 
  match lc with
  | [] -> []
  | [(e,coul)] -> if e = c then [] else [(e,coul)]
  | (e,coul)::suite -> if e = c then suite else (e,coul)::(supprime_dans_lc suite c);; 

(* Supprime une case d'une configuration *)
let supprime_dans_config (conf:configuration) (c:case) : configuration =
  let (lc,lcoul,d) = conf in ((supprime_dans_lc lc c), lcoul, d);;

(* Assertion pour tester la fonction supprime_dans_config *)
assert(supprime_dans_config conf1 (-4,2,2)
       =([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
          ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
          ((-4, 1, 3), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
          ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
          ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
          ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)], [Vert; Rouge; Jaune], 3));;
affiche conf1;;
affiche (supprime_dans_config conf1 (-4,2,2));;

(* Q19 *)

(* Vérifie si un coup est valide dans une configuration *)
let est_coup_valide (conf:configuration) (cp:coup) : bool =
  let (lc,lcoul,d) = conf in
  match cp with
  | (Du(c1,c2)) -> (sont_cases_voisines c1 c2) && ((quelle_couleur c1 conf) = List.hd lcoul) && ((quelle_couleur c2 conf) = Libre) && (est_dans_losange c2 d) 
  | Sm(lc) -> failwith "Sauts multiples non implémentés";;

(* Assertion pour tester la fonction est_coup_valide *)
assert(est_coup_valide conf1 (Du((-4, 1, 3), (-3, 1, 2)))=true);;

assert(est_coup_valide conf1 (Du((-4, 1, 3), (-2, 1, 1)))=false);;

(* Q20 *)

(* Cette fonction applique un coup à une configuration donnée *)
let appliquer_coup (conf:configuration) (cp:coup) : configuration = 
  match cp with
  | Du(c1, c2) ->
      let conf = supprime_dans_config conf c1 in
      let (lc, lcoul, d) = conf in
      (lc @ colorie (List.hd lcoul) [c2], lcoul, d)
  | Sm lc -> failwith "Sauts multiples non implémentés";; 

affiche conf2;;
affiche (appliquer_coup conf2 (Du((-4, 1, 3), (-3, 1, 2))));;

(* Assertion pour tester la fonction appliquer_coup *)
assert(appliquer_coup conf1 (Du((-4, 1, 3), (-2, 1, 1)))=
       ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
         ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
         ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
         ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
         ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
         ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-2, 1, 1), Vert)],
        [Vert; Rouge; Jaune], 3));;

(* Q21 *)

(* Cette fonction met à jour la configuration en appliquant un coup *)
let mettre_a_jour_configuration (conf:configuration) (cp:coup) : configuration =
  if (est_coup_valide conf cp) = false then failwith "Ce coup n'est pas valide, le joueur doit rejouer" 
  else (appliquer_coup conf cp);;

(* Assertion pour tester la fonction mettre_a_jour_configuration *)
assert(mettre_a_jour_configuration conf1 (Du((-4, 1, 3), (-3, 1, 2)))=
       ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
         ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
         ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
         ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
         ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
         ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-3, 1, 2), Vert)],
        [Vert; Rouge; Jaune], 3));;

(* Q22 *)

(* Cette fonction vérifie si un segment entre deux cases est libre *)
let rec est_libre_seg (c1:case) (c2:case) (conf:configuration) : bool =
  let (vect,d) = (vec_et_dist c1 c2) in 
  match d with
  | 0 -> false
  | 1 -> true
  | a -> let case_suiv = (translate c1 vect) in ((quelle_couleur case_suiv conf) = Libre) && (est_libre_seg case_suiv c2 conf);;

(* Assertions pour tester la fonction est_libre_seg *)
assert(est_libre_seg (-4, 1, 3) (-1, 1, 0) conf1 = true);;

assert(est_libre_seg (-4, 1, 3) (-1, 1, 0) conf11 = false);;

(* Q23 *)

(* Cette fonction vérifie si un saut entre deux cases est possible *)
let est_saut (c1:case) (c2:case) (conf:configuration) : bool = 
  let (lc, lcoul, d) = conf in 
  match calcul_pivot c1 c2 with
  | None -> false
  | Some p -> 
      (quelle_couleur p conf <> Libre) &&
      (est_libre_seg c1 p conf) &&
      (est_libre_seg p c2 conf) && 
      (quelle_couleur c2 conf = Libre) &&
      (quelle_couleur c1 conf = List.hd lcoul);; 

affiche conf12;;
assert (est_saut (0,2,-2) (0,-2,2) conf12 = true);;
assert (est_saut (0,2,-2) (0,-1,1) conf12 = false);;  

(*Q24*)

let rec est_saut_multiple(lc:case list)(conf:configuration):bool= 
  match lc with
  |[]-> failwith "Liste vide"
  |[c]-> failwith "Au moins deux cases"
  |c1::c2::[]-> (est_saut c1 c2 conf)
  |c1::c2::suite->if (est_saut c1 c2 conf) then (est_saut_multiple (c2::suite) (appliquer_coup conf (Du(c1,c2)))) else false;;
  
(* Tests *)
affiche conf13;;
assert(est_saut_multiple [(-3, 0, 3); (-5, 2, 3); (-3, 2, 1); (-1, 2, -1); (1, 2, -3)] conf13 = true);; 
affiche conf14;;
assert(est_saut_multiple [(-5, 2, 3); (-2, 2, 0); (0, 0, 0)] conf14 = false);; 

(* Q25 *)

(* Vérifie si un coup est valide dans une configuration donnée *)
let est_coup_valide (conf: configuration) (cp: coup): bool =
  let (lc, lcoul, d) = conf in
  match cp with
  | Du(c1, c2) ->
      (* Vérifie si les cases sont voisines, si la couleur de la première case est la couleur du joueur, si la deuxième case est libre et si elle est dans le losange *)
      (sont_cases_voisines c1 c2) && ((quelle_couleur c1 conf) = List.hd lcoul) && ((quelle_couleur c2 conf) = Libre) && (est_dans_losange c2 d) 
  | Sm(lc) ->
      (* Vérifie si le saut multiple est valide *)
      (est_saut_multiple lc conf) && (est_dans_losange (der_liste lc) d);;

(* Tests *)
affiche conf15;;
assert(est_coup_valide conf15 (Sm([(-4, 2, 2); (0, -2, 2); (2, -2, 0)]))=true);; 
affiche conf16;;
assert(est_coup_valide conf16 (Du((-4, 1, 3), (-2, 1, 1)))=false);;

(* Applique un coup à une configuration donnée *)
let appliquer_coup (conf: configuration) (cp: coup) : configuration = 
  match cp with
  | Du(c1, c2) ->
      (* Supprime la première case et ajoute la deuxième coloriée *)
      let conf = supprime_dans_config conf c1 in
      let (lc, lcoul, d) = conf in
      (lc @ colorie (List.hd lcoul) [c2], lcoul, d)
  | Sm(l) ->
      (* Si c'est un saut multiple, applique la premiere et derniere cases comme un saut normal *)
      appliquer_coup conf (Du((List.hd l), (der_liste l)));;
(* Tests *)
affiche conf13;;
affiche (appliquer_coup conf13 (Sm([(-3,0,3);(-5,2,3);(-3,2,1);(-1,2,-1);(1,2,-3)])));; 
affiche conf1;;
affiche (appliquer_coup conf1 (Du((-4, 1, 3), (-2, 1, 1))));;
assert(appliquer_coup conf1 (Du((-4, 1, 3), (-2, 1, 1)))=
       conf17);;
affiche conf17;;

(* Met à jour la configuration en appliquant un coup *)
let mettre_a_jour_configuration (conf: configuration) (cp: coup): configuration =
  (* Vérifie si le coup est valide, sinon lève une exception *)
  if (est_coup_valide conf cp) = false then
    failwith "Ce coup n'est pas valide, le joueur doit rejouer" 
  else
    (appliquer_coup conf cp);;

(* Tests *)
assert(mettre_a_jour_configuration ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
                                     ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
                                     ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); 
                                     ((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); 
                                     ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
                                     ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)],
                                    [Vert;Rouge;Jaune],3) (Du((-4, 1, 3), (-3, 1, 2)))=
       ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
         ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
         ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
         ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune);
         ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
         ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-3, 1, 2), Vert)],
        [Vert; Rouge; Jaune], 3));;

(* Q26 *)
  
(* Donne le score des pions d'un certain joueur *)
let score (conf: configuration): int=
  let (lc, lcoul, d) = conf in let couleur = List.hd lcoul in 
  let liste_scores = (List.map (fun ((i, j, k), coul) -> if coul = couleur then i else 0) lc) in
  List.fold_right (+) liste_scores 0;;
  
(* Tests *)
assert(score conf1 = -28);; 
assert(score ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
               ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
               ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
               ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-2, 1, 1), Vert)], 
              [Vert; Rouge; Jaune], 3) = -26);;

(* Affiche le score du protagoniste s'il a gagné *)
let score_gagnant (dim: dimension): int=
  let conf = (remplir_triangle_haut dim (dim+1, -dim, -1)) in
  let liste_scores = (List.map (fun (i, j, k) -> i) conf) in
  List.fold_right (+) liste_scores 0;;

(* Tests *)  
assert(score_gagnant 1 = 2);;
assert(score_gagnant 2 = 10);;
assert(score_gagnant 3 = 28);;
assert(score_gagnant 4 = 60);;

(* Q27 *)

(* Renvoie un booléen si un protagoniste a gagné *)
let gagne (conf: configuration): bool=
  let (lc, lcoul, d) = conf in
  (score conf) = (score_gagnant d);;

(* Tests *)
assert(gagne ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge); 
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((4, -1, -3), Vert); ((4, -2, -2), Vert); ((4, -3, -1), Vert); 
               ((5, -2, -3), Vert); ((5, -3, -2), Vert); ((6, -3, -3), Vert); 
               ((3, 1, -4), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);
               ((2, 2, -4), Jaune); ((2, 3, -5), Jaune); ((1, 3, -4), Jaune)],
              [Vert;Rouge;Jaune],3) = true);; 
assert(gagne ([((3, -6, 3), Rouge); ((3, -5, 2), Rouge); ((3, -4, 1), Rouge);
               ((2, -5, 3), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge);
               ((-4, 2, 2), Vert); ((-4, 3, 1), Vert); ((-5, 2, 3), Vert);
               ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((3, 1, -4), Jaune); 
               ((3, 2, -5), Jaune); ((3, 3, -6), Jaune); ((2, 2, -4), Jaune);
               ((2, 3, -5), Jaune); ((1, 3, -4), Jaune); ((-2, 1, 1), Vert)], 
              [Vert; Rouge; Jaune], 3) = false);;

(* Q28 *)

(* Fonctions auxiliaires qui donne la configuration du protagoniste suivante et mettre a jour le coup joué et donne la couleur du joueur gagnant *)
let new_config conf cp = (tourner_config (mettre_a_jour_configuration conf cp));;
let couleur_gagnant conf = let (lc,lcoul,d)=conf in List.hd lcoul;;
   
(* Fonction qui prend une configuration et applique tous les coups et s'il exisite un gagnant il envoie son couleur *)              
let rec est_partie (conf: configuration) (lcp: coup list): couleur= 
  match lcp with
  |[] -> if (gagne conf) then couleur_gagnant conf else Libre
  |e::suite -> if (gagne (mettre_a_jour_configuration conf e)) then couleur_gagnant conf else (est_partie (new_config conf e) suite);;

(* Tests *)
assert (est_partie conf1 [Du((-4, 3, 1), (-3, 2, 1));
                          Du((-4, 2, 2), (-3, 1, 2));
                          Du((-4, 3, 1), (-3, 3, 0));
                          Du((-3, 2, 1), (-2, 2, 0));
                          Sm([(-5, 3, 2); (-1, -1, 2)]);
                          Sm([(-5, 3, 2); (-1, 3, -2); (-1, 1, 0)]);
                          Du((-2, 2, 0), (-1, 1, 0));
                          Sm([(-1, -1, 2); (1, -1, 0); (1, 1, -2)]);
                          Sm([(-3, 3, 0); (1, -1, 0)]);
                          Du((-4, 1, 3), (-3, 0, 3));
                          Sm([(-6, 3, 3); (-2, 3, -1)]);
                          Du((-4, 1, 3), (-3, 0, 3));
                          Du((-3, 0, 3), (-2, -1, 3));
                          Du((-2, 3, -1), (-2, 2, 0));
                          Sm([(-1, 1, 0); (3, -3, 0)]);
                          Sm([(-1, 1, 0); (-1, -1, 2)]);
                          Du((-3, 1, 2), (-2, 0, 2));
                          Sm([(1, -1, 0); (3, -1, -2)]);
                          Du((-1, -1, 2), (0, -1, 1));
                          Du((-2, 0, 2), (-1, -1, 2));
                          Sm([(-4, 2, 2); (2, 2, -4); (2, -2, 0); (4, -2, -2)]);
                          Du((-5, 3, 2), (-4, 3, 1));
                          Sm([(-2, 2, 0); (0, 0, 0)]);
                          Sm([(-5, 2, 3); (3, 2, -5); (3, 0, -3)]);
                          Du((-4, 3, 1), (-3, 3, 0));
                          Du((-4, 1, 3), (-3, 0, 3));
                          Du((3, -1, -2), (4, -1, -3));
                          Du((-3, 3, 0), (-2, 3, -1));
                          Sm([(-5, 2, 3); (-1, -2, 3)]);
                          Sm([(3, 0, -3); (5, -2, -3)]);
                          Du((-4, 2, 2), (-4, 1, 3));
                          Du((-3, 0, 3), (-3, 1, 2));
                          Du((-6, 3, 3), (-5, 3, 2));
                          Du((-5, 2, 3), (-5, 3, 2));
                          Du((-4, 3, 1), (-4, 2, 2));
                          Sm([(-5, 3, 2); (1, 3, -4); (1, 1, -2); (1, -1, 0); (1, -3, 2); (5, -3, -2)]);
                          Du((-6, 3, 3), (-5, 2, 3));
                          Du((1, 1, -2), (2, 1, -3));
                          Du((-3, 0, 3), (-2, -1, 3));
                          Du((-5, 2, 3), (-6, 3, 3));
                          Du((-4, 2, 2), (-4, 1, 3));
                          Sm([(-2, -1, 3); (0, -3, 3); (2, -3, 1); (4, -3, -1); (6, -3, -3)]);
                          Du((-4, 1, 3), (-3, 1, 2));
                          Sm([(-3, 1, 2); (1, 1, -2)]);
                          Du((3, -3, 0), (4, -3, -1))] = Jaune);;

assert (est_partie ([((2, -1, -1), Jaune); ((-2, 1, 1), Vert)], [Vert; Jaune], 1) [Du((-2, 1, 1), (-1, 1, 0));
                                                                                   Du((-2, 1, 1), (-1, 1, 0));
                                                                                   Du((-1, 1, 0), (0, 1, -1));
                                                                                   Du((-1, 1, 0), (-1, 0, 1))] = Libre);;

(* Q29 *)

(*let case_suiv (c:case) (d:direction): case =
   let (i, j, k) = c in 
   match d with
   |NE -> (i+1, j, k-1) 
   |NO -> (i+1, j-1, k)
   |E -> (i, j+1, k-1)
   |O -> (i, j-1, k+1)
   |SE -> (i-1, j+1, k)
   |SO -> (i-1, j, k+1);;*)




















  
  
  

