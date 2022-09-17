# pokedex

Command line program written in Haskell that takes a Pokémon's name as input and returns some information about that Pokémon as the output. Retrieves information from
[PokéAPI](https://pokeapi.co/).


## Building/Running

With GHC, cabal and stack installed, the library can be built by running `stack build` in a Terminal window.

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
