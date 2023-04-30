# Projet Cours Intelligence Artificielle et Programmation Symbolique

## Description

Ce projet a pour but de mettre en place un bot pour expliquer les règles du jeu du tour de France ainsi qu'une IA pour jouer au jeu.

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
  cp front/.env.example front/.env
  ```

  Modifier le fichier `.env` pour configurer les variables d'environnement, la variable par défaut est correct si vous ne touchez pas au serveur prolog.

## Développement

- Lancer le serveur front

  ```bash
  cd front
  npm run dev
  ```

- Lancer le serveur de l'IA

  ```bash
  cd back
  swipl -s server.pl
  ```

## Déploiement

- Build le front

  ```bash
  cd front
  npm run build
  ```

- Lancer le serveur

  ```bash
  cd back
  swipl -s server.pl
  ```

- Accéder au site

  Website : <http://localhost:5173> | Bot : <http://localhost:3000>
