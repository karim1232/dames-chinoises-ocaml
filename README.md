# ⭐ Dames chinoises

Moteur du jeu de **dames chinoises** écrit en **OCaml** fonctionnel : plateau, coups, sauts multiples, détection du gagnant et simulation de parties complètes.

## 🧭 Représentation du plateau

Le plateau en étoile à 6 branches utilise des **coordonnées cubiques** `(i, j, k)` avec `i + j + k = 0`. Avec ce système :
- une **rotation de 60°** revient à permuter les coordonnées et changer leurs signes
- deux cases sont **alignées** si elles ont une coordonnée en commun
- un **déplacement** est une simple addition de vecteurs

## ✨ Ce qui est implémenté

- **Géométrie** : appartenance au losange et à l'étoile, rotation, translation, voisinage, case pivot d'un saut, vecteur et distance entre deux cases
- **Position de départ** pour 1, 2, 3 ou 6 joueurs, construite en faisant tourner le triangle du premier joueur
- **Affichage du plateau en ASCII** dans le terminal
- **Coups** :
  - `Du (c1, c2)` : déplacement d'une case
  - **saut** par-dessus un pion, symétrique par rapport au pion sauté (le chemin doit être libre)
  - `Sm [c1; c2; …]` : **sauts multiples** enchaînés
- **Validation** des coups, puis **rotation du plateau** pour passer au joueur suivant
- **Score** de chaque joueur et **détection de la victoire**
- **Simulation d'une partie** à partir d'une liste de coups, qui renvoie le gagnant

Chaque fonction est vérifiée par des **tests `assert`**.

## 🚀 Lancer

```bash
ocaml dames_chinoises.ml
```

Ou dans l'interpréteur OCaml :

```ocaml
#use "dames_chinoises.ml";;
```

Au chargement, les tests sont vérifiés et des plateaux d'exemple s'affichent.

## 🧠 Notions utilisées

Types somme et produit, filtrage par motif, récursivité sur les listes, fonctions d'ordre supérieur (`List.map`, `List.fold_right`), type `option`.

## 👤 Auteur

Karim Glelah
