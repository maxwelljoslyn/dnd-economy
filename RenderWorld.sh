# create renderer executable
ghc --make -O2 WorldRenderer.hs 

# run executable
./WorldRenderer -o $1.svg -w 1000
