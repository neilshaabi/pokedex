{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module PokemonDataTypes where

import Data.Text (Text)
import Data.Aeson ((.:), FromJSON(parseJSON), Value(Object, Null))
import GHC.Generics (Generic)


-- | Data type to store information retrieved from Pokemon API
data Pokemon = Pokemon
    { name   :: Text           -- ^ Name of Pokemon
    , height :: Int            -- ^ Height of Pokemon in decimetres
    , types  :: [PokemonType]  -- ^ List of types the Pokemon has
    } deriving (Show, Generic)

instance FromJSON Pokemon

-- | @newtype@ wrapper over NamedAPIResource storing a single Pokemon type
newtype PokemonType = PokemonType 
    { type' :: NamedAPIResource
    } deriving (Show)

{- | Instance declaration defined explicitly to permit parsing of JSON key @type@
(reserved word in Haskell, hence @type'@ is used as the record field name) -}
instance FromJSON PokemonType where
    parseJSON (Object v) = PokemonType <$> v .: "type"

-- | @newtype@ wrapper over Text storing the name field of a Named API Resource
newtype NamedAPIResource = NamedAPIResource 
    { name' :: Text
    } deriving (Show, Generic)

{- | Instance declaration defined explicitly to handle name clashes in record fields 
(@name@ appears in Pokemon data type) and null keys (e.g. @habitat@ can be null) -}
instance FromJSON NamedAPIResource where
    parseJSON (Object v) = NamedAPIResource <$> v .: "name"
    parseJSON Null = pure (NamedAPIResource "none")

-- | Data type to store information retrieved from Pokemon Species API
data PokemonSpecies = PokemonSpecies
    { flavor_text_entries :: [FlavorText]      -- ^ List of flavor text entries for this Pokémon species
    , habitat             :: NamedAPIResource  -- ^ Habitat this Pokémon species can be encountered in
    , is_legendary        :: Bool              -- ^ Whether or not this is a legendary Pokémon
    } deriving (Show, Generic)

instance FromJSON PokemonSpecies

-- | Data type to store the flavor text for an API resource
data FlavorText = FlavorText
    { flavor_text :: Text              -- ^ Flavor text for an API resource
    , language    :: NamedAPIResource  -- ^ Language this flavor text is in
    } deriving (Show, Generic) 

instance FromJSON FlavorText