# World Map and Economy System

## Introduction

I run D&D (no group at the moment since I've been moving a lot in recent years). I use the computer to construct various artifacts to help me run D&D. These are a gameworld map and a set of tables giving price information for many objects which the players can sell and purchase.

These tools are a work in progress. They don't work end-to-end yet. But they do produce some stuff. The map engine, in particular, can produce a map, so that might be fun to look at.

These programs are not made for anyone but me. Also, I am an amateur programmer. For these reasons, do not expect the code to conform to engineering standards or to use the most sophisticated methods. I learn as I go.

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

#### Acknowledgments

My economy system is based on that described by [Alexis Smolensk](http://tao-dnd.blogspot.ca/), who is in my opinion the world's most important D&D blogger. [His wiki](http://tao-of-dnd.wikispaces.com/Trade+System) goes into detail about how to set up your own system of this kind. I've been working on this project since before he wrote those tutorials (there were other, fragementary versions 

If you're a DM and you don't want to be as committed as Alexis, you're not a DM.
