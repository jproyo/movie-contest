# Movie Contest

[![Build Status](https://travis-ci.org/jproyo/movie-contest.svg?branch=master)](https://travis-ci.org/jproyo/movie-contest.svg?branch=master)

## Rules of the game

- If the program know 3 movies in a row it wins
- If the user can name a movie it doesn't know the user wins
- After a winner is crowned all the mentioned movies should be listed ordered by release year

```
    Sample outputs
    Computer wins
    I bet you can't tell me a movie I don't know in 3 tries!
    Name a movie
    the silence of the lambs
    I know it: The Silence of the Lambs
    A young F.B.I. cadet must confide in an incarcerated and manipulative killer to receive his help on catching another serial killer who skins his victims.

    Name a movie
    rocky
    I know it: Rocky
    Rocky Balboa, a small-time boxer, gets a supremely rare chance to fight the heavy-weight champion, Apollo Creed, in a bout in which he strives to go the distance for his self-respect.

    Name a movie
    the godfather
    I know it: The Godfather
    The aging patriarch of an organized crime dynasty transfers control of his clandestine empire to his reluctant son.

    I win!
    The movies I knew about:
    The Godfather from 1972
    Rocky from 1976
    The Silence of the Lambs from 1991

    User wins
    I bet you can't tell me a movie I don't know in 3 tries!
    Name a movie
    titanic
    I know it: Titanic
    A seventeen-year-old aristocrat falls in love with a kind but poor artist aboard the luxurious, ill-fated R.M.S. Titanic.

    Name a movie
    that movie with arnold and the small guy as his brother
    I'm beaten. I don't know that movie.
    The movies I knew about:
    Titanic from 1997
```

## Solution

### Prerequisites

In order to run this solution you are going to need the following distributions installed.

- Stack 1.6.5+

### Run tests

```shell
bash.$ stack test
```

### Run Solution

```shell
bash.$ stack build
bash.$ OMDB_API_KEY=XXXX stack exec movie-contest
```

*where XXXX is your OMDB API KEY*


### Run on Docker

You need to have Docker 1.8+ installed in your host machine

1. If you dont have docker stackage machine pulled first you need to pull from dockerhub

```shell
bash.$ stack docker pull
```

2. Once this is done you can repeat the following recipe to build and run in a docker container the solution

```shell
bash.$ stack --docker build
bash.$ OMDB_API_KEY=XXXX stack --docker exec movie-contest
```
