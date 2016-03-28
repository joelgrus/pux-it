module PuxIt where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (RANDOM(), random)
import Control.MonadPlus (guard)
import Data.Array (length, zip, (..), elemIndex, replicateM, sortBy)
import Data.Array.Unsafe (unsafeIndex, head)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, Tuple(..))
import Prelude (class Show, bind, (&&), (||), show, (==), return, ($), map, const, (++), (-), (>>=), compare, otherwise)
import Pux (start, renderToDOM, EffModel())
import PuxIt.Math (createDeck)
import Pux.Html (Html)
import Pux.Html.Elements (img, div)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (src, className, alt)

-- Type aliases for all of our primitives to make the code more readable.
type Image     = Int          -- An image is just represented as an integer.
type Card      = Array Image  -- A card is just an array of images.
type Deck      = Array Card   -- A deck is just an array of cards
type CardIndex = Int          -- indexed by an integer.

-- Each image corresponds to an image file, as follows. (Note that for a game of
-- size `n` you'll have n^2 + n + 1 images, so you'll need that many image files.
-- In our case we'll be using n = 7 => 57 images, like images/0.svg, etc...)
imageUrl :: Image -> String
imageUrl image = "images/" ++ show image ++ ".svg"

-- And the only Action we have is to click on a card. The Click action will
-- return the index of the clicked card.
data Action = Click CardIndex

-- We don't really need this, but it's helpful for debugging.
instance showAction :: Show Action where
  show (Click i) = "Click " ++ show i

-- You can select 0, 1, or 2 cards. We store their index[es].
data SelectedCards = NoCards | OneCard CardIndex | TwoCards CardIndex CardIndex

type State = {
  cards    :: Deck,               -- The cards in the deck, in order.
  selected :: SelectedCards       -- Which cards (indexes) are selected
}

-- This is not the most efficient way to shuffle, but it's simple and it works.
-- Pair each element with a random, sort by the randoms, and then throw them away.
shuffle :: forall e a. Array a -> Eff (random :: RANDOM | e) (Array a)
shuffle xs = do
  randoms <- replicateM (length xs) random
  return $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

-- Create a random deck, by first shuffling the cards, then shuffling the
-- images on each card. Because `shuffle` is an effectful operation on each card
-- we use `traverse` to collect the effects into an effectful array.
createRandomDeck :: forall e. Int -> Eff (random :: RANDOM | e) Deck
createRandomDeck n = shuffle (createDeck n) >>= traverse shuffle

-- Get the initial state for a game, with a shuffled deck. The supplied `n`
-- should be prime, or else the cards won't satisfy the "any two cards match"
-- property.
initialState :: forall e. Int -> Eff (random :: RANDOM | e) State
initialState n = do
  cards <- createRandomDeck n
  return { cards : cards, selected : NoCards }

-- Finds the common image on cards[i] and cards[j]. Implicitly assumes that
-- there is exactly one such image, which there will be if the deck is
-- constructed correctly.
commonImage :: Deck -> CardIndex -> CardIndex -> Image
commonImage cards i j = head $ do
  image1 <- cards `unsafeIndex` i
  image2 <- cards `unsafeIndex` j
  guard $ image1 == image2
  return image1

-- Logic to update the state after the player clicks on a card. There's probably
-- a cleaner way to write this. But the idea is that you can have 0, 1, or 2
-- cards "selected". If you click on a selected card, that unselects it. If you
-- click on an unselected card, that selects it, unless there's already 2 cards
-- selected, in which case it does nothing.
cardClicked :: CardIndex -> State -> State
cardClicked i state = state { selected = toggle state.selected }
  where
    toggle NoCards                      = OneCard i       -- select card i
    toggle (OneCard s) | s == i         = NoCards         -- unselect
                       | otherwise      = TwoCards s i    -- select second card
    toggle (TwoCards s1 s2) | i == s1   = OneCard s2      -- unselect s1
                            | i == s2   = OneCard s1      -- unselect s2
                            | otherwise = TwoCards s1 s2  -- no op

-- The `update` function is very simple, since there's only one action.
update :: Action -> State -> EffModel State Action (random  :: RANDOM)
update (Click i) state = { state: cardClicked i state, effects: [] }

-- like `map` but the function depends on the value and the index
mapWithIndex :: forall a b. (a -> Int -> b) -> Array a -> Array b
mapWithIndex f xs = map (\(Tuple x i) -> f x i) $ zip xs (0 .. (length xs - 1))

-- The `view` is a `div` that contains the results of mapping `renderCard`
-- over the cards and their indexes.
view :: State -> Html Action
view state = div [] cardsHtml
  where
    cardsHtml = mapWithIndex (renderCard state.selected correctImage) state.cards
    correctImage = case state.selected of
      TwoCards i j -> Just (commonImage state.cards i j)
      _            -> Nothing

-- We render a card as a `div` that contains all of its rendered `images`.
-- We add a click handler that returns the card's index. And if the card is
-- selected, we add a "selected" class.
renderCard :: SelectedCards -> Maybe Image -> Card -> CardIndex -> Html Action
renderCard selectedCards correctImage card i =
  div [ className cardClass, onClick cardClick ] cardHtml
  where
    isSelected = case selectedCards of
      NoCards        -> false
      OneCard s      -> s == i
      TwoCards s1 s2 -> s1 == i || s2 == i
    cardClass = if isSelected then "card selected" else "card"
    -- We don't care about the click event itself, so we use `const` to ignore it.
    cardClick = const (Click i)
    cardHtml = map (renderImage isSelected correctImage) card

-- We render an image as just an `img` tag pointing to the relevant `src` url.
-- If this image's card is selected, and if this is the correct image (based on
-- two selected cards), we add a "correct" class to it.
renderImage :: Boolean -> Maybe Image -> Image -> Html Action
renderImage isSelected correctImage image =
  img [ src url, alt altText, className imageClass ]
  where
    url = imageUrl image
    altText = show image
    imageClass = if isCorrectImage then "image correct" else "image"
    isCorrectImage = case correctImage of
      Just correct -> isSelected && image == correct
      otherwise    -> false
