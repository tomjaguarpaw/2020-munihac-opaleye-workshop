# Opaleye workshop &ndash; Munihac 2020

## When is the workshop?

Saturday, September 12th at 15:00–18:00 CEST.  See [the Munihac
website](https://munihac.de/2020.html) for any announcements of last
minute updates to the scheduling.

## Prerequisites

* To get the most out of this workshop you should be familiar with at
least intermediate-level Haskell, including "do" notation.

* If you want to follow along by writing your own code examples you
should know how to clone a Haskell git repository and build it with
Cabal (or other build tool of your choice).

* If you want to *run* your own code examples you should have an
installation of Postgres available.  The tutorial will use
[tmp-postgres](https://hackage.haskell.org/package/tmp-postgres-1.34.1.0)
to ensure it does not interfere with any other Postgres database you
may have.

* Familiarity with the list monad would be particularly helpful for
understanding Opaleye's semantics.  Familiarity with SQL will also be
useful.

## What will the workshop involve?

* We will work through some of the examples on
  <https://www.postgresqltutorial.com/>.

* I will answer anyone's questions about Opaleye and we will discuss
  anything about Opaleye that attendees would like to discuss.
  Suggested topics for questions and discussion include

  * Implementation/internals
  * Denotational semantics
  * Testing strategy (property testing)
  * How
    [`Default`](https://hackage.haskell.org/package/product-profunctors-0.11.0.0/docs/Data-Profunctor-Product-Default.html)/[product-profunctors](https://github.com/tomjaguarpaw/product-profunctors) work
  * Philosophy

## How should I prepare for the workshop?

The following instructions will work on Linux.  The will probably
mostly work on Mac and may need adjustment for anyone using Windows.

* Make sure you have GHC >= 8.6 and a recent version of Cabal

  If you don't have them you might want to obtain them using
  [ghcup](https://www.haskell.org/ghcup/).

* DO THIS STEP AS EARLY AS POSSIBLE!

  Install and build the example database.  Building `aeson` (one of
  the dependencies) can take a long time so it would be a good idea to
  do this part well before the workshop.

  ```
  $ cabal v2-update
  $ git clone https://github.com/tomjaguarpaw/2020-munihac-opaleye-workshop.git
  $ cd 2020-munihac-opaleye-workshop
  $ cabal v2-build
  ```

* Make sure you have Postgres installed (you can do this in parallel
  with the previous step)

  For example on Debian

  ```
  sudo apt-get install postgresql-11
  ```

* Download the sample database from [PostgreSQL Sample
  Database](https://www.postgresqltutorial.com/postgresql-sample-database/)
  (you can do this in parallel with the previous steps)

  ```
  $ cd /tmp
  $ curl --output dvdrental.zip https://sp.postgresqltutorial.com/wp-content/uploads/2019/05/dvdrental.zip
  $ unzip dvdrental.zip
  ```

* Run the smoke test (once all the previous steps are complete)

  In the `2020-munihac-opaleye-workshop` directory

  ```
  $ cabal v2-repl
  *Main> smokeTest
  Starting temporary DB...restoring data...connecting...smoke test was successful
  Result: ()
  ```

  If anything goes wrong with the smoke test you can report it on the
  workshop Slack channel or [file an
  issue](https://github.com/tomjaguarpaw/2020-munihac-opaleye-workshop/issues).

## What's next?

More details will be announced here before the talk, probably by
Friday, September 11th.
