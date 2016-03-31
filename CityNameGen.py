import random

consonants = ["b","p","s","c","d","t","f","g","h","x","j","y","k","l","m","n",
             "q","r","v","w","z"]
vowels = ['a','e','i','o','u']

def replaceConsonant(region):
    return random.choice(getRegionalConsonants(region))

def replaceVowel():
    return random.choice(vowels)

def fillPattern(pattern,region):
    result = []
    for letter in pattern:
        if letter == "C":
            result.append(replaceConsonant(region))
        elif letter == "V":
            result.append(replaceVowel())
        elif letter == "$":
            # reduplication mark: repeat last letter
            index = len(result) - 1
            result.append(result[index])
        else:
            # pass the input through
            # useful for prefixes, glottal stops, etc
            result.append(letter)
    return "".join(result)

def getRegionalConsonants(region):
    """Picks the consonants which a given region will use for its markets.
    The argument is an integer."""
    result = []
    l = len(consonants)
    offset = 2
    bot = region - offset
    top = region + offset
    result.append(consonants[region])
    # the idea is to return those letters
    # which are at these indices in consonants list:
    # region, and the range around region given by
    #moving up or down the list by offset
    if bot < 0:
        # find the index to which we should continue,
        # after "wrapping around" to end of list
        wrapToTop = abs(bot)
        for x in consonants[(l-wrapToTop):l]:
            result.append(x)
    else:
        # normal treatment of bot
        for x in consonants[bot:region]:
            result.append(x)
    if top > l:
        # find the index to which we should continue,
        # after "wrapping around" to beginning of list
        wrapToBottom = top - l
        for x in consonants[0:wrapToBottom]:
            result.append(x)
    else:
        # normal treatment of top
        for x in consonants[region:top]:
            result.append(x)
    return result

def regionSoundPatterns(region):
    """Returns the sound patterns used in a region's words.
    This is very basic and can be expanded for more variety,
    most obviously by giving each region number its own separate case."""
    universals = ["VCV","CV"]
    if region <= 4:
        return universals + ["Vn", "CVn", "Vng", "CVng"]
    elif region <= 8:
        return universals + ["rV","r'V", "Vr", "CVr"]
    elif region <= 11:
        return universals + ["kV","Vk","CVk","kVC"]
    elif region == 12:
        # special case because region 12 is really big;
        # I don't want its patterns showing up anywhere else,
        # lest things get monotonous
        return universals + ["mV$", "mV", "V$","VC", "Vm", "CVm"]
    elif region <= 15:
        return universals + ["dV", "Vd", "dVC","CVd"]
    elif region <= 18:
        return universals + ["gV", "gV$", "V$", "Vg", "V$g"]
    else:
        return universals + ["V-V"]

def maxPatternsPerWord(region):
    """Maximum number of sound patterns in one word,
    for the argument region."""
    return (region % 2) + 2

def makeWordPattern(region):
    """Returns a word pattern based on the region's sound characteristics."""
    # number to use
    numPatterns = random.randint(2,maxPatternsPerWord(region))
    i = 0
    result = []
    while i < numPatterns:
        result.append(random.choice(regionSoundPatterns(region)))
        i += 1
    return "".join(result)

def makeWord(region):
    """Make a word based on a pattern."""
    base = fillPattern(makeWordPattern(region),region)
    # upcase the first letter of the word
    return str.upper(base[0]) + base[1:]
                
def makeMarketName(region):
    """Returns a generated city name for the argument region,
    according to the relevant sound patterns and maximum patterns."""
    # either one or two words per name
    numWords = random.randint(1,2)
    result = []
    result.append(makeWord(region))
    if numWords == 2:
        # space between words!
        result.append(" ")
        result.append(makeWord(region))
    return "".join(result)
