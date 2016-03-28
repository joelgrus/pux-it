module Main where

import Prelude
import Pux (start, renderToDOM)
import PuxIt (initialState, update, view)

main = do
  state <- initialState 7  -- you could change this, if you have enough images
  app <- start {
    initialState: state
  , update: update
  , view: view
  , inputs: []
  }

  renderToDOM "#app" app.html
