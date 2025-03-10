# Overseerr Auto Request

![build workflow status](https://github.com/imatpot/overseerr-auto-request/actions/workflows/build.yml/badge.svg)
![publish workflow status](https://github.com/imatpot/overseerr-auto-request/actions/workflows/publish.yml/badge.svg)

[Overseerr](https://overseerr.dev/) is a fantastic application, which allows your friends to request movies and TV series for your [Plex Server](https://www.plex.tv/).
However, it lacks a built-in way to automatically re-requests specific titles once they get denied.

This application solves that problem!
Simply configure the movies and TV shows you want to keep requesting, run the program, and let it handle the rest.
No more manual re-requesting—just set it and forget it!

Building and running comes in three-ish falvours, mentioned below accordingly.

## Build

You can build the auto requester using Cabal, Nix, and Docker.

### Cabal

If you have [Cabal](https://www.haskell.org/cabal/) installed, you can build the binary with

```
cabal build
```

### Nix

If you have [Nix](https://nixos.org) installed, you can build the derivation with

```
nix run
```

You don't even need to pull the repo to do this, just run

```
nix run github:imatpot/overseerr-auto-request
```

on your machine to build and run the auto requester.

### Docker

If you have [Docker](https://www.docker.com/) installed, you can build the image with

```
docker build -t overseerr-auto-request .
```

or

```
docker compose build
```

## Run

As with building, you can run the auto requester using Cabal, Nix, and Docker.

### Cabal

If you have [Cabal](https://www.haskell.org/cabal/) installed, you can run the auto requester with

```
cabal run
```

### Nix

If you have [Nix](https://nixos.org) installed, you can run the auto requester with

```
nix run
```

Again, use

```
nix run github:imatpot/overseerr-auto-request
```

to run the auto requester without pulling the repo.

### Docker

If you have [Docker](https://www.docker.com/) installed and you built the image, you can run the auto requester with

```
docker run overseerr-auto-request
```

or, alternatively, you can use Docker Compose:

```
docker compose up
```

## Environment Variables

The auto requester requires several environment variables to run.
Copy the `.env.schema` file to `.env` and fill in the values, or set them accordingly in your environment.

| Variable           | Description                                                           | Required | Default |
| ------------------ | --------------------------------------------------------------------- | :------: | ------- |
| `EMAIL`            | The email address you use to locally log in to Overseerr.             |    🚩     |         |
| `PASSWORD`         | The password you use to locally log in to Overseerr.                  |    🚩     |         |
| `OVERSEERR_URL`    | The URL of your Overseerr instance.                                   |    🚩     |         |
| `MOVIES`           | A comma-separated list of movie titles you want to keep requesting.   |          |         |
| `TV_SHOWS`         | A comma-separated list of TV show titles you want to keep requesting. |          |         |
| `DEBOUNCE_SECONDS` | The number of seconds to wait between request batches.                |          | 300     |

## Debugging

For debugging purposes, the auto requester comes with an HTTP-sink.
This sink allows you to inspect the requests your sending by redirecting them to your localhost.

Here's how to run the sink:

```
cabal run http-sink   # using Cabal
nix run .#http-sink   # using Nix
```

You cannot run the sink in Docker.
If you really want to run it in Docker, do yourself a favour and just run an [Nginx](https://nginx.org/) container instead.
