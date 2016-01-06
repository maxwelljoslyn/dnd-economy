# generate input
python3 WorldGenerator.py

# create renderer executable
ghc --make -O2 WorldRenderer.hs 

# run executable
./WorldRenderer -o WorldMap.svg -w 1000
