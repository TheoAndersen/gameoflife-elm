# gameoflife-elm
Game of life implementation in Elm

This is my learning attempt at a Game of life implementation in elm.

I had to change my approach underway, when i found that just doing a nested list of Cells and having to check on them all on every tick was way to expensive.

The current implementation works, but gets slow pretty quickly when you add more live cells

Features
- Ticker that runs 2x a second
- Able to press every square to make them alive (to add a little fun)
