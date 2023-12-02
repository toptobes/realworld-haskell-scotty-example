# ![RealWorld Example App](logo.png)

> ### Haskell codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://demo.realworld.io/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **Haskell/Scotty** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell/Scotty** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# How it works

## Basic architectural overview
Since this is small application (only 2k+ϵ actual LOC), I've opted for a very vertical-slice-esque architecture, with each endpoint
getting its own file, and with common logic simply going in its own files. Controllers, services, and data access are of course still
decoupled through MTL style classes (inspired by [three-layer-cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)),
making testing extremely simple. I believe the general architecture should be quite self explanitory and easy to refactor as the app scales.
I will admit it *is* slightly influenced by Haskell's dissallowance of circular imports, and I couldn't get hls to work with an hs-boot file.

Feel free to let me know your thoughts, or open an issue!

View more fine-grained documentation [here](https://toptobes.github.io/realworld-scotty-hs/).

## Noteworthy dependencies
(Not including common dependencies such as `mtl` and `aeson`)
 - [`scotty`](https://github.com/scotty-web/scotty) — The minimal web "framework" which makes this all possible
 - [`relude`](https://github.com/kowainik/relude) — A nicer/safer Prelude alternative
 - [`esqueleto`](https://github.com/bitemyapp/esqueleto) — A type-safe SQL eDSL wrapping [`persistent`](https://www.yesodweb.com/book/persistent)
 - [`jwt`](https://hackage.haskell.org/package/jwt) — Library for working w/ JWTs
 - [`cryptonite`](https://hackage.haskell.org/package/cryptonite) — Low-level cryptography library
 - [`wai-middleware-static`](https://hackage.haskell.org/package/wai-middleware-static) — Used to easily serve static files
 - [`file-embed`](https://hackage.haskell.org/package/file-embed) — Used to embed the sqlbits directly into haskell code

## Basic file-structure overview
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

  Errors.hs               # Some code for handling and translating
                          # feature-specific errors

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

## Misc
**On ExceptT vs Exceptions:** yeah uh, besides the lack of structural typing, from my non-expert opinion, one of my biggest issues with Haskell
is the lack of truly idiomatic error handling; ask 10 different people, get 11 different answers. I ended up going with ExceptT because it seemed like
the simplest method while maintaining sufficient cleanliness and typechecking.

**On not just writing into the AppM monad:** I easily could've used `(AppM m)` as a constraint directly, everywhere. And some people would argue that I
should've, on an app on this scale—which is totally fair: it's much simpler and lightweight, compared to the (somewhat?) three-layer-style I opted for.
However, I do find semantic meaning in seeing some context such as `(PasswordGen m, AuthTokenGen m, CreateUser m, ReadUsers m)` where it's immediately
clear what the function needs access to. It's also arguably better for testing, but I didn't really test the services here (due to the tests provided
by gothinkster), so I won't go into that here. It's arguable that such contexts are merely implementation details, but I'd say that it's more than
a detail; it's a purpose, the essence of that function. But that's just me, do whatever you want lol.

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
 - If necessary, you can set the `CONDUIT_CONFIG` env var to to some custom config path

Then, you can start the app via `cabal run app`, or run the unit tests w/ `cabal run spec`.

It may take a while the first time while it obtains/builds the relevant dependencies.

If you plan to make any modifications involving creating new files or adding new dependencies, you may need [hpack](https://github.com/sol/hpack).
 - You could update the `realworld-hs.cabal` file manually if you really wanted or needed to though
 - Otherwise, just run the `hpack` command whenever you modify `package.yaml`

## Further documentation

**Access the documentation site @ [https://toptobes.github.io/realworld-scotty-hs/].**
Nearly everything except for the features are decently documented, and I'll work on adding more soon.

You can also build the documentation site locally using `cabal haddock`, and then serve it w/ `npx serve` or whatever
else you fancy.

Or just read through the code manually if you prefer, whatever you want lol.
