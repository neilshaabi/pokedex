{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import System.IO (hFlush, stdout)
import Data.Text (Text, toLower, replace, unpack, pack)
import Network.Wreq (JSONError, asJSON, get, responseBody)
import Network.HTTP.Client (HttpException)
import Control.Lens ((^.))
import Control.Exception (catches, Handler (Handler))

import PokemonDataTypes

import qualified Data.Text.IO as T (putStr, putStrLn, getLine)

{- | Prompts user to enter Pokemon name via command-line, calls function to
retrieve data if name is valid, otherwise restarts -}
main :: IO ()
main = do
    T.putStr "\nEnter a Pokémon name: "
    hFlush stdout
    input <- T.getLine
    getPokemon (toLower input) `catches` [
        Handler (\(e :: HttpException) -> printErrorMsg "Pokémon not found"),       
        Handler (\(e :: JSONError)     -> printErrorMsg "No name entered")]

-- | Calls functions to retrieves and output information for a given Pokemon name
getPokemon :: Text -> IO ()
getPokemon name = do
    pokemonInfo <- callPokemonAPI name
    speciesInfo <- callSpeciesAPI name
    printPokemon pokemonInfo speciesInfo

-- | Retrieves information from Pokemon API, returns it in a @Pokemon@ type
callPokemonAPI :: Text -> IO Pokemon
callPokemonAPI name = do
    r <- asJSON =<< get (unpack $ "https://pokeapi.co/api/v2/pokemon/" <> name)
    pure (r ^. responseBody)

-- | Retrieves information from Pokemon Species API, returns it in a @PokemonSpecies@ type
callSpeciesAPI :: Text -> IO PokemonSpecies
callSpeciesAPI name = do
    r <- asJSON =<< get (unpack $ "https://pokeapi.co/api/v2/pokemon-species/" <> name)
    pure (r ^. responseBody)

-- | Outputs Pokemon information retrieved from both APIs to command-line
printPokemon :: Pokemon -> PokemonSpecies -> IO ()
printPokemon Pokemon{..} PokemonSpecies{..} =
    T.putStrLn ("- name: "          <> name                                         <>
                "\n- description: " <> replaceNewlines firstEnglishDesc             <>
                "\n- height: "      <> pack (show height) <> " decimetres"          <>
                "\n- types: "       <> pack (show [ name' $ type' t | t <- types ]) <>
                "\n- habitat: "     <> name' habitat                                <>
                "\n- isLegendary: " <> pack (show is_legendary)                     <>
                "\n")
        where
            -- | Returns the first English description for this Pokemon
            firstEnglishDesc :: Text
            firstEnglishDesc = head [ flavor_text t | t <- flavor_text_entries, name' (language t) == "en" ]

            -- | Replaces newline and form feed characters with spaces in a given @Text@
            replaceNewlines :: Text -> Text
            replaceNewlines text = foldr (\c -> replace c " ") text ["\n", "\f"]

-- | Prints error message when input is invalid, prompts user to re-enter name
printErrorMsg :: Text -> IO ()
printErrorMsg msg = do
    T.putStrLn (msg <> ", please try again.")
    main