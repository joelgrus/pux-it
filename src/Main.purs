module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

import PuxIt (initialState, foldp, view)

main :: forall fx. Eff (CoreEffects (random :: RANDOM | fx)) Unit
main = do
  state <- initialState 7  -- you could change this, if you have enough images
  app <- start {           -- but it *must* be a prime number
    initialState: state
  , view: view
  , foldp: foldp
  , inputs: []
  }

  renderToDOM "#app" app.markup app.input
