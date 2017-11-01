module ColourSets where

import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Palette.BrewerSet

-- http://www.color-hex.com/color-palettes/?page=124

type Colours = [Colour Double]


toC = map sRGB24read



allColours = [ (pastelSorrows,         "pastelSorrows")
             , (bellyache,             "bellyache")
             , (sunriseToSunset,       "sunriseToSunset")
             , (differentFlavors,      "differentFlavors")
             , (brightPurplesAndPinks, "brightPurplesAndPinks")
             , (twoFaux,               "twoFaux")
             , (jamaicanDutchBeach,    "jamaicanDutchBeach")
             , (visionZeroOriginal,    "visionZeroOriginal")
             , (ribbons,               "ribbons")
             , (bn34,                  "bn34")
             , (bn31,                  "bn31")
             , (depoiss,               "depoiss")
             , (sana,                  "sana")
             , (hugo,                  "hugo")
             , (atTheBeach,            "atTheBeach")
             , (takoTank,              "takoTank")
             , (incaTawantinsuyo,      "incaTawantinsuyo")
             , (sunsetPassion,         "sunsetPassion")
             , (instagramGradient,     "instagramGradient")
             , (freshMint,             "freshMint")
             , (mujeresDelChaco,       "mujeresDelChaco")
             , (sunset80s,             "sunset80s")
             , (soulmates,             "soulmates")
             , (fruit,                 "fruit")
             , (fiesta,                "fiesta")
             , (christmas,             "christmas")
             , (carnaval,              "carnaval")
             , (theSunSetsInTheTropic, "theSunSetsInTheTropic")
             , (junina,                "junina")
             , (gypsyStick,            "gypsyStick")
             , (artPop,                "artPop")
             , (brewerSet3_12,         "brewerSet3_12")
             , (loveEachOther,         "loveEachOther")
             , (redGrapeFruit,         "redGrapeFruit")
             , (royalVictoria,         "royalVictoria")
             , (funkPalace,            "funkPalace")
             , (helphy,                "helphy")
             , (lisaFrank,             "lisaFrank")
             , (bhuvansGroupo,         "bhuvansGroupo")
             , (summertime,            "summertime")
             , (tvDefault,             "tvDefault")
             , (cartoonNetwork,        "cartoonNetwork")
             , (meaningful,            "meaningful")
             , (redGalaxy,             "redGalaxy")
             , (aibril,                "aibril")
             , (alwaysWhite,           "alwaysWhite")
             ]
            

-- The original design.
brewerSet3_12 :: Colours
brewerSet3_12 = brewerSet Set3 12


-- http://www.color-hex.com/color-palette/49416
pastelSorrows :: Colours
pastelSorrows = toC [ "ffcece"
                    , "a4dcfc"
                    , "fbfca4"
                    , "a0f19c"
                    , "8295fc"
                    ]


-- http://www.color-hex.com/color-palette/49417
bellyache :: Colours
bellyache = toC [ "ff0000"
                , "f4ff73"
                , "023bff"
                , "c2fffd"
                , "c900bd"
                ]


-- http://www.color-hex.com/color-palette/49393
sunriseToSunset :: Colours
sunriseToSunset = toC [ "e65c6f"
                      , "f98d77"
                      , "ffcb69"
                      , "ffe269"
                      , "fffa85"
                      ]


-- http://www.color-hex.com/color-palette/48976
differentFlavors :: Colours
differentFlavors = toC [ "f60b0b"
                       , "78f299"
                       , "5752f4"
                       , "fbf680"
                       , "c40ce7"
                       ]


-- http://www.color-hex.com/color-palette/48426
brightPurplesAndPinks :: Colours
brightPurplesAndPinks = toC [ "7c00ff"
                            , "ab17ff"
                            , "db2df0"
                            , "ff00ce"
                            , "ff00a7"
                            ]


-- http://www.color-hex.com/color-palette/45653
twoFaux :: Colours
twoFaux = toC [ "11d871"
              , "56e3d0"
              , "f5f426"
              , "f6a30d"
              , "d90097"
              ]


-- http://www.color-hex.com/color-palette/45041
jamaicanDutchBeach :: Colours
jamaicanDutchBeach = toC [ "60af62"
                         , "ecee3c"
                         , "f6f5ee"
                         , "6ab6c6"
                         , "ffcead"
                         ]


-- http://www.color-hex.com/color-palette/44919
visionZeroOriginal :: Colours
visionZeroOriginal = toC [ "01b8aa"
                         , "fd625e"
                         , "374649"
                         , "5658e1"
                         , "8f7ded"
                         ]


-- http://www.color-hex.com/color-palette/44761
ribbons :: Colours
ribbons = toC [ "ffd87b"
              , "daa0f4"
              , "ffcfff"
              , "d3fff9"
              , "aba3ff"
              ]


-- http://www.color-hex.com/color-palette/44750
bn34 :: Colours
bn34 = toC [ "9a55d9"
           , "d554d2"
           , "d54c7f"
           , "d9ac49"
           , "50afd2"
           ]


-- http://www.color-hex.com/color-palette/44746
bn31 :: Colours
bn31 = toC [ "d50065"
           , "c200d3"
           , "5a00d7"
           , "009ed5"
           , "00d4b8"
           ]


-- http://www.color-hex.com/color-palette/44638
depoiss :: Colours
depoiss = toC [ "79ecaf"
              , "64ec7b"
              , "ff8e88"
              , "ff9b36"
              , "b23fdb"
              ]


-- http://www.color-hex.com/color-palette/44602
sana :: Colours
sana = toC [ "0a2d76"
           , "002cc9"
           , "b6b4b4"
           , "dadeea"
           , "f68141"
           ]


-- http://www.color-hex.com/color-palette/44601
hugo :: Colours
hugo = toC [ "3cafc7"
           , "ff6d6d"
           , "ffffff"
           , "36c781"
           , "5056a9"
           ]


-- http://www.color-hex.com/color-palette/44511
atTheBeach :: Colours
atTheBeach = toC [ "ffdf86"
                 , "ffc952"
                 , "008aff"
                 , "1d00ff"
                 , "a6fff7"
                 ]


-- http://www.color-hex.com/color-palette/44491
takoTank :: Colours
takoTank = toC [ "2560fe"
               , "a200ff"
               , "ff4040"
               , "ffa500"
               , "6dc066"
               ]


-- http://www.color-hex.com/color-palette/44503
incaTawantinsuyo :: Colours
incaTawantinsuyo = toC [ "ce1717"
                       , "ffbd00"
                       , "f6f6f6"
                       , "1f7e21"
                       , "732e98"
                       ]


-- http://www.color-hex.com/color-palette/44492
sunsetPassion :: Colours
sunsetPassion = toC [ "ff5500"
                    , "aa0011"
                    , "770044"
                    , "440044"
                    , "000044"
                    ]


-- http://www.color-hex.com/color-palette/44340
instagramGradient :: Colours
instagramGradient = toC [ "feda75"
                        , "fa7e1e"
                        , "d62976"
                        , "962fbf"
                        , "4f5bd5"
                        ]


-- http://www.color-hex.com/color-palette/44325
freshMint :: Colours
freshMint = toC [ "46edc8"
                , "374d7c"
                , "ff5978"
                , "fbb35a"
                , "fdf289"
                ]


-- http://www.color-hex.com/color-palette/44302
mujeresDelChaco :: Colours
mujeresDelChaco = toC [ "2d5e49"
                      , "2eb4dc"
                      , "f2cb4e"
                      , "da3333"
                      , "b43c7e"
                      ]


-- http://www.color-hex.com/color-palette/44263
sunset80s :: Colours
sunset80s = toC [ "e100a7"
                , "55ebfc"
                , "ffc1ec"
                , "ffd939"
                , "ff6127"
                ]


-- http://www.color-hex.com/color-palette/44251
soulmates :: Colours
soulmates = toC [ "377cdb"
                , "a228a9"
                , "e7707c"
                , "d0ba5f"
                , "b46dff"
                ]


-- http://www.color-hex.com/color-palette/44211
fruit :: Colours
fruit = toC [ "11c300"
            , "ffa4b3"
            , "ff1313"
            , "ffe132"
            , "25a519"
            ]


-- http://www.color-hex.com/color-palette/44209
fiesta :: Colours
fiesta = toC [ "7f73ff"
             , "ffe132"
             , "14d0f0"
             , "ff2f81"
             , "11c300"
             ]


-- http://www.color-hex.com/color-palette/44202
christmas :: Colours
christmas = toC [ "ec1313"
                , "ff1313"
                , "25a519"
                , "eaeaea"
                , "dadada"
                ]


-- http://www.color-hex.com/color-palette/44190
carnaval :: Colours
carnaval = toC [ "ffe132"
               , "ff1313"
               , "ff9936"
               , "14d0f0"
               , "11c300"
               ]


-- http://www.color-hex.com/color-palette/44201
theSunSetsInTheTropic :: Colours
theSunSetsInTheTropic = toC [ "951ebf"
                            , "6761e7"
                            , "64d2f6"
                            , "fbadcd"
                            , "fbc49e"
                            ]


-- http://www.color-hex.com/color-palette/44184
junina :: Colours
junina = toC [ "7f73ff"
             , "0b40e7"
             , "3fc95f"
             , "ff9936"
             , "ec1313"
             ]


-- http://www.color-hex.com/color-palette/44150
gypsyStick :: Colours
gypsyStick = toC [ "fff600"
                 , "33b3a6"
                 , "800080"
                 , "ff3663"
                 , "aaaaaa"
                 ]

artPop :: Colours
artPop = toC [ "ffd0e8"
             , "f49be3"
             , "e76ed0"
             , "a91e95"
             , "6f1d6c"
             ]

-- http://www.color-hex.com/color-palette/44172
loveEachOther :: Colours
loveEachOther = toC [ "d090f4"
                    , "ec9a9f"
                    , "fbbe95"
                    , "fffb92"
                    , "fffcac"
                    ]

-- http://www.color-hex.com/color-palette/44161
redGrapeFruit :: Colours
redGrapeFruit = toC [ "dc3257"
                    , "a23e54"
                    , "563952"
                    , "4a3156"
                    , "593c66"
                    ]


-- http://www.color-hex.com/color-palette/44090
royalVictoria :: Colours
royalVictoria = toC [ "0030ff"
                    , "ffd400"
                    , "ffffff"
                    , "fffa7b"
                    , "5ccfff"
                    ]


-- http://www.color-hex.com/color-palette/44038
funkPalace :: Colours
funkPalace = toC [ "d270f4"
                 , "00255f"
                 , "5560d1"
                 , "8c62e9"
                 , "2e7bff"
                 ]

-- http://www.color-hex.com/color-palette/43911
helphy :: Colours
helphy = toC [ "236e96"
             , "15b2d3"
             , "5abccc"
             , "ffbe42"
             , "ff7f00"
             ]


-- http://www.color-hex.com/color-palette/43875
lisaFrank :: Colours
lisaFrank = toC [ "e9008d"
                , "f6841b"
                , "fcea08"
                , "6eb63f"
                , "05aded"
                ]


-- http://www.color-hex.com/color-palette/43852
bhuvansGroupo :: Colours
bhuvansGroupo = toC [ "ffc900"
                    , "00c1ff"
                    , "ff0400"
                    , "d63000"
                    , "0014a7"
                    ]


-- http://www.color-hex.com/color-palette/43747
summertime :: Colours
summertime = toC [ "236e96"
                 , "15b2d3"
                 , "ffd700"
                 , "f3872f"
                 , "ff598f"
                 ]



-- http://www.color-hex.com/color-palette/43581
tvDefault :: Colours
tvDefault = toC [ "f2e45b"
                , "66ffcd"
                , "59ff75"
                , "fe50ff"
                , "e33838"
                ]


-- http://www.color-hex.com/color-palette/43581
cartoonNetwork :: Colours
cartoonNetwork = toC [ "fad400"
                     , "ff00c2"
                     , "00c2ff"
                     , "000000"
                     , "ffffff"
                     ]


-- http://www.color-hex.com/color-palette/43575
meaningful :: Colours
meaningful = toC [ "72ccff"
                 , "5ffc8e"
                 , "54ffbb"
                 , "ff4d4d"
                 , "ff499f"
                 ]


-- http://www.color-hex.com/color-palette/43518
redGalaxy :: Colours
redGalaxy = toC [ "b8334c"
                , "f56458"
                , "f2ac6f"
                , "454595"
                , "2b1321"
                ]


-- http://www.color-hex.com/color-palette/43360
aibril :: Colours
aibril = toC [ "fdb913"
             , "1c9ad6"
             , "009793"
             , "ed145b"
             , "782b90"
             ]


alwaysWhite :: Colours
alwaysWhite = toC [ "ffffff" ]
