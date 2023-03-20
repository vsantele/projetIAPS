# Projet Cours Inteligence Artificielle et programmation symbolique

## Description

Ce projet a pour but de mettre en place un bot pour expliquer les règles du jeu du tour de france ainsi qu'une IA pour jouer au jeu.

## Installation

### Prérequis

- [NodeJS](https://nodejs.org) (version 18)
- [SWI-Prolog](https://www.swi-prolog.org)

### Etapes

- Cloner le projet

  ```bash
  git clone https://github.com/vsantele/projetIAPS/
  ```

- Installer les dépendances

  ```bash
  cd front
  npm install
  ```

- Configurer les variables d'environnement

  ```bash
  cp .env.example .env
  ```

  Modifier le fichier `.env` pour configurer les variables d'environnement

## Développement

- Lancer le serveur

  ```bash
  cd front
  npm run dev
  ```

- Lancer le serveur de l'IA

  ```bash
  cd back
  swipl -s server.pl
  ```

  Dans prolog:

  ```prolog
  ?- start_server.
  ```

## Déploiement

- build le front

  ```bash
  cd front
  npm run build
  ```

- Lancer le serveur

  ```bash
  cd back
  swipl -s server.pl
  ```

  Dans prolog:

  ```prolog
  ?- start_server.
  ```
