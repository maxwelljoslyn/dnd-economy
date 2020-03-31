# World Map and Economy System

## Introduction

I run D&D. I use the computer to construct various artifacts to help me run D&D. These are a gameworld map and a set of tables giving price information for many objects which the players can sell and purchase.

These tools are a work in progress. At this time they do work end-to-end: my program goes all the way from market town info to producing a pricing table for any of those towns, and the map program generates a map for some or all of the hemisphere.

These programs are not made for anyone but me. At this time I am not a professional programmer but I try to write good clear code, and make small commits.

I fully expect to work on this project for years, but I do take breaks to work on the other aspects of my D&D world. A website or repo for those rules and tools is in the works.

If you want to contact me, do it at maxwelljoslyn AT gmail DOT com.

## Program Information

These programs are written in Python 3 and Haskell. I greatly prefer Haskell, but at the time of starting I still had a lot of trouble doing stuff involving random generation in that language. It is also difficult to replicate many standard algorithms which assume mutable reference (but check back when I've had a chance to study the ST monad.) Thus I currently use Python to generate the world information, and then either import the results into other Python files for further work in that language, or write it into structured text files for parsing into Haskell programs.

For now, I am focused on adding to the content of the pricing system.

## You Will Need

### For Python

You'll need the noise library located [here](https://pypi.python.org/pypi/noise/).

One day I might figure out how to save the noise lib for you locally, so you don't have to download it manually and install it system-wide, but I ran into complications doing that myself. Perhaps you'd like to show me how in a pull request?

### For Haskell

The parsers use the [Parsec](https://hackage.haskell.org/package/parsec) library, and the renderer uses the [Diagrams](http://projects.haskell.org/diagrams/) library. You can get both of these through `cabal`, as usual. I am using Diagrams 1.3 and Parsec 3.1.5.

Eventually I will update the Haskell portion of the codebase to support Stack, like a real project; then you won't have to manually install these libraries, nor worry about versions.

## Usage Instructions

### To Output Recipe Prices

Run `python3 RecipeRunner.py` to get prices written both to standard output, and to a file `Prices.html`.

### To Build World Data and Emit it for Rendering

Simply run `python3 WorldGenerator.py`

### To Run the Map Renderer

The shell script `RenderWorld.sh`, which requires an argument name for its output, will compile the renderer, then run the resulting executable to produce the output SVG image. The name of the output will be whatever you passed as an argument, plus the `.svg` extension. The resulting image size will be approximately 20 MB if the whole map is rendered, and less if you only render a portion of it.

#### Acknowledgements

The economy system which I am working towards is based on the one described by [Alexis Smolensk](http://tao-dnd.blogspot.ca/).  Alexis's [wiki](http://tao-of-dnd.wikispaces.com/Trade+System) goes into detail about how to set up your own system of this kind.

I read his earlier work on his system some time before setting out to work on my own project, so most of the economy concepts originate with him. My primary theoretical innovation, which prompted me to begin working, was to have one interconnected system where a single "backend" data generator or store powered the creation of both the world map *and* the output of trade tables for player-purchasable items. As of early June 2016 I have achieved this goal of creating the system. Now I am in the process of expanding and improving it, with an eye toward making it capable of handling some kind of real Earth data in the future. 

In my opinion Alexis is the world's most important D&D blogger. If you want to know why, just read his blog. Other DMs don't research. Other DMs complain about working on their games. Other DMs use storebought rulesets that don't even halfway describe any kind of gameable, coherent world. By contrast, Alexis *works*, and he writes about his work, and his craftmanship shows through in every new table or page or post he puts up.
