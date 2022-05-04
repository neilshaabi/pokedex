# pokedex

Command line program written in Haskell that takes a Pokémon's name as input and returns some information about that Pokémon as the output. Retrieves information from
[PokéAPI](https://pokeapi.co/).


## Building/Running

With GHC, cabal and stack installed, the library can be built by running `stack build` in a Terminal window while in the `/pokedex` directory.

The program can then be run locally using the command: `stack run`.


## Usage

To retrieve information for a Pokémon, the user must enter its name when prompted as a single string with **no punctuation** (refer to the examples below). If the name entered does not match the name of an existing Pokémon (or if no name is entered), an error message will be displayed and the user will be prompted to retry.

Note that the Pokémon names accepted by the program are **case-insensitive** - information will be retrieved for a Pokémon as long as its name is spelled correctly, regardless of its capitalisation.

```
Enter a Pokémon name: pikachu
- name: pikachu
- description: When several of these POKéMON gather, their electricity could build and cause lightning storms.
- height: 4 decimetres
- types: ["electric"]
- habitat: forest
- isLegendary: False

Enter a Pokémon name: lion
Pokémon not found, please try again.

Enter a Pokémon name: ArTiCuNo
- name: articuno
- description: A legendary bird POKéMON that is said to appear to doomed people who are lost in icy mountains.
- height: 17 decimetres
- types: ["ice","flying"]
- habitat: rare
- isLegendary: True
```

## Possible extensions

Given the task of designing a production/live application (or given additional time), I would allow the user to interact with the program through a **web application**, rather than the command-line. For example, entering a Pokémon name could render an HTML webpage with the same information accompanied by an image of the Pokémon, which I believe would be a better experience for the user.

Additionally, I would modify the data types in `PokemonDataTypes.hs` to store additional information retrieved from each PokéAPI call. With this functionality implemented, I would give the user the option to **select/deselect specific fields** as well as the languages of the descriptions that they would like to be displayed, promoting a sense of personalisation and improving the relevance of the data from the user's perspective.
