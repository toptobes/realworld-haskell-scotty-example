# ![RealWorld Example App](logo.png)

> ### Haskell codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://demo.realworld.io/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **Haskell/Scotty** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell/Scotty** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# How it works

## Basic architectural overview
Since this is small application (only ~2k actual LOC), I've opted for a very vertical-slice-esque architecture, with each endpoint
getting its own file, with common logic simply going in its own files. Controllers, services, and data access are of course still
decoupled through MTL style classes making testing simple (though I've not sprung for many tests due to the Cypress suite already being
provided for us). I believe the general architecture should be quite self explanitory and easy to refactor. I will admit it *is* slightly 
influenced by Haskell's dissallowance of circular imports, and I didn't want to deal with hs-boot files.

Feel free to let me know your thoughts, or open an issue!

## File-structure overview
```ruby
app/                      # The entrypoint into the application.
  Main.hs                 # Very thin, just deals with configuration.

sqlbits/                  # Some sql files w/ triggers/functions
                          # which are embedded directly within
                          # the haskell files via TemplateHaskell.

src/                      # The actual source code for Conduit.

  Conduit/App/            # This folder deals with the App Monad,
                          # which holds the server's global state.

  Conduit/DB/             # Holds some DB-related utilities and
                          # code for decoupling/abstraction purposes.
                          # Also holds some DB initializtion code.

  Conduit/Features/       # Contains the bulk of the Conduit logic,
                          # including the API endpoints/services/DB
                          # access logic.
                          # Also holds feature-related DB/error logic.

  Conduit/Identity/       # Holds the code for the JWT-based auth.
                          # uses argon2 for pw hashing.

  Validation.hs           # Some utilities for basic validation
                          # of incoming data.

static/                   # Holds the static files where the avatar
                          # images reside. The images are actually just
                          # blank files for now but it's fiiine.

test/                     # Contains the unit tests made w/ hspec,
                          # currently only have tests for auth and
                          # slug-building since cypress covers the rest.

package.yaml              # Describes the project and its dependencies.
realworld-hs.cabal        # hpack generates the .cabal file from the
                          # package.yaml which, IMO, is much nicer
                          # to work with.

conduit-schema.json       # The configuration files for conduit itself. 
conduit.json              # The schema file for your convenience.
```

## Noteworthy dependencies
(Not including common dependencies such as `mtl` and `aeson`)
 - [`scotty`](https://github.com/scotty-web/scotty) — The minimal web "framework" which makes this all possible
 - [`relude`](https://github.com/kowainik/relude) — A nicer/safe Prelude alternative
 - [`esqueleto`](https://github.com/bitemyapp/esqueleto) — A type-safe SQL eDSL wrapping [`persistent`](https://www.yesodweb.com/book/persistent)
 - [`jwt`](https://hackage.haskell.org/package/jwt) — Library for working w/ JWTs
 - [`cryptonite`](https://hackage.haskell.org/package/cryptonite) — Low-level cryptography library
 - [`wai-middleware-static`](https://hackage.haskell.org/package/wai-middleware-static) — Used to easily serve static files

# Getting started

This project requires Cabal & a running Postgres instance. Here're a couple links of that may help you out:
 - [Cabal via GHCup](https://cabal.readthedocs.io/en/3.4/getting-started.html) — Guide for installing + getting started w/ Cabal
 - [Postgres + Docker](https://www.youtube.com/watch?v=G3gnMSyX-XM) — A quick tutorial for getting a Postgres container running

To begin, of course, clone the repo and cd into it:
```julia
git clone https://github.com/toptobes/realworld-scotty-hs.git && cd realworld-scotty-hs
# or
gh repo clone toptobes/realworld-scotty-hs && cd realworld-scotty-hs
```

From there, update the appropriate configs in `conduit.json`
 - It's already set with sensible defaults; be sure to double-check the postgres connection string
 - there is a `conduit-schema.json` for your convenience as well that provides extra info

Then, you can start the app via `cabal run app`, or run the unit tests w/ `cabal run spec`.

It may take a while the first time while it obtains/builds the relevant dependencies.

If you plan to make any modifications involving creating new files or adding new dependencies, you may need [hpack](https://github.com/sol/hpack).
 - You could update the `realworld-hs.cabal` file manually if you really wanted or needed to though
 - Otherwise, just run the `hpack` command whenever you modify `package.yaml`
