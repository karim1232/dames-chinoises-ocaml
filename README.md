# Dames chinoises

Moteur du jeu de dames chinoises écrit en OCaml, en style fonctionnel. Il gère le plateau, les coups, les sauts multiples, la détection du gagnant et la simulation de parties complètes.

## Le plateau

Le plateau en étoile à 6 branches est repéré en coordonnées cubiques `(i, j, k)`, avec `i + j + k = 0`. Ce choix simplifie beaucoup de choses : une rotation de 60° se fait en permutant les coordonnées et en changeant leurs signes, deux cases sont alignées quand elles ont une coordonnée en commun, et un déplacement est une addition de vecteurs.

## Ce qui est implémenté

- Géométrie : appartenance au losange et à l'étoile, rotation, translation, voisinage, case pivot d'un saut, vecteur et distance entre deux cases
- Position de départ pour 1, 2, 3 ou 6 joueurs, obtenue en faisant tourner le triangle du premier joueur
- Affichage du plateau en ASCII dans le terminal
- Coups :
  - `Du (c1, c2)` : déplacement d'une case
  - saut par-dessus un pion, symétrique par rapport au pion sauté (le chemin doit être libre)
  - `Sm [c1; c2; …]` : plusieurs sauts enchaînés
- Validation des coups, puis rotation du plateau pour passer au joueur suivant
- Score de chaque joueur et détection de la victoire
- Simulation d'une partie à partir d'une liste de coups, qui renvoie le gagnant

Chaque fonction a ses tests `assert`.

## Lancer

```bash
ocaml dames_chinoises.ml
```

ou depuis l'interpréteur OCaml :

```ocaml
#use "dames_chinoises.ml";;
```

Au chargement, les tests tournent et quelques plateaux d'exemple s'affichent.

## Notions utilisées

Types somme et produit, filtrage par motif, récursivité sur les listes, fonctions d'ordre supérieur (`List.map`, `List.fold_right`), type `option`.

## Auteur

Karim Glelah
