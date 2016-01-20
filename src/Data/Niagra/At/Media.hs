{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.Media
(
  media,
  media'
)
where

import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.DSL
import Data.Niagra.Builder (Builder)

import Data.Text (Text)

-- |A @media query.
media :: (Monad m)
      => Bool -- ^ True=only, False=not
      -> Text -- ^ media type
      -> [(Text,Builder)] -- ^ media features
      -> NiagraT m ()-- ^ content of the @media query
      -> NiagraT m ()
media isOnly type_ feat act = rootScope (Media isOnly type_ feat) act
      
-- |See definition and 'media'.
media' :: (Monad m)
       => Text
       -> NiagraT m () -- ^ content of the @media query
       -> NiagraT m ()
media' type_ act = media True type_ [] act