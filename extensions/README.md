# Extensions

Ce dossier contient le projet de base + les extensions suivantes:\

- Interface web pour jouer
- Implémentation cgi en node.js
- Sauvegarde des parties
- IA bataille navale

## Requirements

- nodejs / npm

Installer les packages suivants avec opam:

- yojson
- ppx_deriving_yojson

```bash
opam install yojson ppx_deriving_yojson
```

## Build

```bash
make build
```

## Usage

```bash
npm install
npm run start
```

Puis ouvrir localhost:5000 dans un navigateur.

## Multijoueur

Le multijoueur n'a pas encore été implémenté faute de temps, cela dit il est en cours d'implémentation.
Il devrait fonctionner de la manière suivante:

- un joueur crée une partie et envois le lien a un autre joueur
- l'autre joueur arrive sur la page du jeu et donne un nom
- une fois que les deux joueurs ont placé leur bateaux la partie peut commencer
- chaque joueur sera averti par socket que c'est à son tour de jouer / qu'il doit update son interface pour afficher le coup de l'autre joueur

## Sécurité

l'implémentation de cgi pose un gros problème de sécurité car on peut injecter du code via les paramètres de la query (voir `index.js` pour plus d'informations)

## UI

Pas de css pour l'heure
