# Copilot Instructions for Jass

## Build, Test, and Document

Standard R package workflow using `devtools` and `roxygen2`:

```r
# Load package interactively (replaces install during development)
devtools::load_all()

# Rebuild documentation from roxygen comments
devtools::document()

# Run R CMD CHECK (equivalent to full CI)
devtools::check()

# Install from local source
devtools::install()
```

There is no formal test suite (no `tests/` directory). Validate changes by running `devtools::check()` and exercising the game interactively:

```r
library(Jass)
library(magrittr)
set.seed(42)
g <- new('Game') %>% deal()
trump(g) <- 'Bells'
meld(g)       # check meld scores before first card
g <- play(g)
```

## Architecture

The package models a game of **Cross Jass** (Swiss card game, also called Schieber Jass) using an S4 class hierarchy:

```
Game
 ├── players: list of Player objects
 ├── score: named integer vector (Team 1, Team 2)
 ├── start: integer (which player starts next round)
 └── round: Round
      ├── trump: character (suit name)
      ├── next_player: integer (1–4)
      ├── hands: list of 4 Hand objects (one per player)
      ├── trick: Trick (current trick in progress)
      │    └── played: list of 4 Hand objects (one card each)
      ├── won: list of 2 Hand objects (one per team, accumulates won cards)
      └── history: list of completed Trick objects
```

**Key classes** are defined in `R/Jass_classes.R`. Methods are split by concern:
- `R/hand_methods.R` — `cards()`, `cards<-()`, `trump()`, `trump<-()`, `card_order()`, `card_value()`, translation helpers
- `R/round_methods.R` — `deal()`, `play()`, `next_trick()`, `status()`, `round_history()`
- `R/game_methods.R` — `is_ai()`, `set_human()`
- `R/Cross_Jass.R` — AI strategy functions: `pick_random_valid_card()`, `pick_random_trump()`
- `R/meld_methods.R` — `meld()` (scoring for sequences, four-of-a-kind, marriages)

**Game flow**: `deal()` → set `trump<-` → `meld()` (before first card) → `play()` (loops until human turn or round end) → `next_trick()` (called automatically inside `play()`) → repeat.

**`meld()` scoring rules (Cross Jass):**
- Sequence of 3 same-suit cards: 20 pts; 4 cards: 50 pts; 5+: 100 pts
- Four Unders (Jacks): 200 pts; Four Nines: 150 pts; Four Aces/Kings/Obers/Banners: 100 pts
- Marriage (King + Ober in trump suit): 20 pts

Only the team with the highest individual meld scores their combined team meld points. Tie-break order: points → card count → trump suit. `meld()` returns a list with `player_melds`, `best_per_player`, `winning_player`, and `team_scores`; it does **not** mutate the `Game`/`Round` object — callers are responsible for applying `team_scores` to `game@score`.

## Key Conventions

**S4 classes and generics throughout.** Every public function is either a `setGeneric` + `setMethod` pair or a plain function. Methods dispatch on the class of `obj` (or `x` for replacement generics). New methods for existing generics must use `setMethod`.

**Card abbreviation format**: first letter of suit + face value, e.g. `"BA"` = Bells Ace, `"F9"` = Flowers 9, `"SU"` = Shields Under. This string format is what `play()` and `cards<-` accept as `value`.

**Suits**: `Bells`, `Flowers`, `Shields`, `Acorns` (Swiss/German deck). `suitTranslation()` maps single-letter abbreviations (`L`, `F`, `S`, `C`) to full names.

**Faces**: `A`, `K`, `O` (Ober), `U` (Under), `B` (Banner/10), `9`, `8`, `7`, `6`. Trump changes both card order (U and 9 rank highest) and point values.

**`cards()` is the central accessor.** It filters to `inhand == TRUE`, sorts by trump → lead suit → suit → rank, and is overloaded for `Hand`, `Trick`, `Round`, and `Game`. The replacement form `cards<-` accepts a character vector of abbreviations, a `data.frame`, or a `Hand`; `draw = FALSE` discards instead of draws.

**`invisible()` on mutating methods.** Functions that return a modified object (e.g., `deal`, `play`, `next_trick`) return `invisible(obj)` so the caller must assign the result (`g <- play(g)`).

**Avoid `no visible binding` notes.** When using `dplyr` verbs with bare column names, declare them as `NULL` at the top of the function (e.g., `inhand <- NULL`), matching the existing pattern in the codebase.

**Documentation via roxygen2.** All exported symbols need `@export`. Grouped docs use `@rdname` to collect related generics into one `.Rd` file (e.g., `@rdname hand-methods`). Run `devtools::document()` after any roxygen change; never edit `NAMESPACE` or `man/*.Rd` by hand.

**4-player, 2-team fixed structure.** Players 1 & 3 are Team 1; players 2 & 4 are Team 2. `teams` is always `c('Team 1', 'Team 2', 'Team 1', 'Team 2')`. The 36-card Swiss deck (no 2–5) is hardcoded in `Hand`'s prototype.
