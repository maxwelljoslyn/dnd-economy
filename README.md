# World Map and Economy System

## Introduction

I run D&D (no group at the moment since I've been moving a lot in recent years). I use the computer to construct various artifacts to help me run D&D. These are a gameworld map and a set of tables giving price information for many objects which the players can sell and purchase.

These tools are a work in progress. They don't work end-to-end yet. But they do produce some stuff. The map engine, in particular, can produce a map, so give that a shot.

These programs are not made for anyone but me. Do not expect the code to conform to engineering standards or to use the most sophisticated methods. I learn as I go.

If you want to contact me, do it at maxwelljoslyn AT gmail DOT com.

## Program Information

These programs are written in Python 3 and Haskell. I greatly prefer Haskell, but it is tricky to do some stuff involving random generation in that language, and (as far as I know) impossible to replicate many standard algorithms which assume mutable reference. Thus I use Python to generate the world information, and then either import the results into other Python files for further work in that language, or write it into structured text files for parsing into Haskell programs.

## You Will Need

### For Python

You'll need the noise library located [here](https://pypi.python.org/pypi/noise/).

One day I might figure out how to save it locally for you, so you don't have to download it manually and install it system-wide, but I ran into complications doing that myself.

### For Haskell

The parsers use the [Parsec](https://hackage.haskell.org/package/parsec) library, and the renderer uses the [Diagrams](http://projects.haskell.org/diagrams/) library. You can get both of these from your favorite Haskell package manager. I don't have any information for you about versioning, sorry. However, eventually I will update the Haskell portion of the codebase to support Stack, like a real project; then you won't have to manually install these libraries.


## Usage Instructions

### To Build World Data and Emit it for Rendering

Simply run `python3 WorldGenerator.`

### To Run the Map Renderer

The shell script `RenderWorld.sh` will compile the renderer, then run the resulting executable, rendering to the default output name of `WorldMap.svg`.

### Acknowledgements

The economy system which I am working towards is based on the one described by [Alexis Smolensk](http://tao-dnd.blogspot.ca/).  Alexis's [wiki](http://tao-of-dnd.wikispaces.com/Trade+System) goes into detail about how to set up your own system of this kind.

I'd read his earlier work on his system some time before setting out to work on my own project, but the fact remains that the fundamentals originate with him. My primary innovation, which prompted me to begin working, was to have one interconnected system where a single "backend" data generator powered the creation of both the world map *and* the output of trade tables for player-purchasable items. So that's my goal.

In my opinion Alexis is the world's most important D&D blogger. If you want to know why, just read his blog. Other DMs don't research. Other DMs complain about working on their games. Other DMs use storebought rulesets that don't even halfway describe any kind of gameable world.

By conrrast, Alexis *works*, and he writes about his work, and his craftmanship shows through in every new table or page or post he puts up.
