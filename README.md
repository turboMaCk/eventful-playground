# Eventful-Playground

Just experimentation with [eventful](https://github.com/jdreaver/eventful).

- Software Transactional Memmory for Concurency
- Typesafe Servant server
- Dummy Elm frontend


## Build Server

With haskell-stack installed on your machine:

```
$ stack build
```

and wait...

## Running Server

So far this is just dummy counter that can be incremented and decremented. State is stored in memmory.

```
$ stack exec eventful-playground-exe
```

This will start server on [localhost:8081](localhost:8081).

## Build Elm frontend

Elm 0.0.18 has to be installed on your machine.

```
$ elm-package install --yes && elm-make elm-src/Main.elm
```

## Run Frontend

While server is running open static index compiled by `elm-make`:

```
$ open index.html # MacOS only
```
