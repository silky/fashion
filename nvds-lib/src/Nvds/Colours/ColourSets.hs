{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Nvds.Colours.ColourSets where

import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Palette.BrewerSet
import Diagrams.Backend.CmdLine
import Data.Monoid
import Options.Applicative.Builder 
import Control.Applicative
import Options.Applicative.Types (readerAsk)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- http://www.color-hex.com/color-palettes/?page=176

type Colours = [Colour Double]


-- This let's us read the colour set name as an argument on the command line.
instance Parseable Colours where
    parser = argument (rc) mempty
        where
            rc :: ReadM (Colours)
            rc = readerAsk >>= readColourSetName


readColourSetName :: (Applicative m, Monad m) 
                  => String 
                  -> m (Colours)
readColourSetName name = return colourSet
    where
        namesToColours = M.fromList $ map (\(a,b) -> (b,a)) allColours
        colourSet      = fromMaybe (error "Unknown colour") 
                                   (M.lookup name namesToColours)


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
             , (energyInMotion,        "energyInMotion")
             , (blueShell,             "blueShell")
             , (barbapapa,             "barbapapa")
             , (sleepAlone,            "sleepAlone")
             , (treePoint,             "treePoint")
             , (galaticEnd,            "galaticEnd")
             , (studio0815,            "studio0815")
             , (redCarpet,             "redCarpet")
             , (fairyFloss,            "fairyFloss")
             , (oldTrellisPayne,       "oldTrellisPayne")
             , (spaceShock,            "spaceShock")
             , (wald,                  "wald")
             , (gothamAtSunrise,       "gothamAtSunrise")
             , (dashboardInterns,      "dashboardInterns")
             , (anythingBut,           "anythingBut")
             , (crystalReef,           "crystalReef")
             , (asmShirt,              "asmShirt")
             , (nightLagoon,           "nightLagoon")
             , (projectAI,             "projectAI")
             , (pinkLadies,            "pinkLadies")
             , (ifSheHadABeach,        "ifSheHadABeach")
             , (goldfish,              "goldfish")
             , (sunsetWonder,          "sunsetWonder")
             , (dreamViolet,           "dreamViolet")
             , (nonmeaningful,         "nonmeaningful")
             , (proudMountains,        "proudMountains")
             , (pulpy,                 "pulpy")
             , (girlNextDoor,          "girlNextDoor")
             , (washfastAcid,          "washfastAcid")
             , (waterAndTheWaves,      "waterAndTheWaves")
             , (violetEnergy,          "violetEnergy")
             , (smoothScaling,         "smoothScaling")
             , (logga,                 "logga")
             , (noodles,               "noodles")
             , (frenchFlag,            "frenchFlag")
             , (lire,                  "lire")
             , (pinkCrystals,          "pinkCrystals")
             , (aquaCrystals,          "aquaCrystals")
             , (orangeCherry,          "orangeCherry")
             , (plumToCerulean,        "plumToCerulean")
             , (sunsetAtSea,           "sunsetAtSea")
             , (cleanAndEnergetic,     "cleanAndEnergetic")
             , (lightPastel,           "lightPastel")
             , (everyDayIsNight,       "everyDayIsNight")
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


-- http://www.color-hex.com/color-palette/43060
energyInMotion :: Colours
energyInMotion = toC [ "6e3dbd"
                     , "29c677"
                     , "fff32e"
                     , "bd9f1d"
                     , "ac3a00"
                     ]


-- http://www.color-hex.com/color-palette/43053
blueShell :: Colours
blueShell = toC [ "7dedf5"
                , "67ced5"
                , "65abf3"
                , "6399d1"
                , "aaaaaa"
                ]


-- http://www.color-hex.com/color-palette/43046
barbapapa :: Colours
barbapapa = toC [ "f3b0cf"
                , "000000"
                , "22b14c"
                , "fa91fb"
                , "3fb5e5"
                ]


-- http://www.color-hex.com/color-palette/43030
sleepAlone :: Colours
sleepAlone = toC [ "ffc40c"
                 , "bfa51e"
                 , "80872f"
                 , "406841"
                 , "004953"
                 ]


-- http://www.color-hex.com/color-palette/43027
treePoint :: Colours
treePoint = toC [ "32d69b"
                , "48a881"
                , "5f7b68"
                , "754e4e"
                , "8c2034"
                ]


-- http://www.color-hex.com/color-palette/43026
galaticEnd :: Colours
galaticEnd = toC [ "08b1d6"
                 , "419aa6"
                 , "798477"
                 , "b16d47"
                 , "ea5617"
                 ]


-- http://www.color-hex.com/color-palette/42987
studio0815 :: Colours
studio0815 = toC [ "4285f4"
                 , "34a853"
                 , "fbbc05"
                 , "ea4335"
                 , "f1f1f1"
                 ]


-- http://www.color-hex.com/color-palette/42941
redCarpet :: Colours
redCarpet = toC [ "ff004d"
                , "ef004d"
                , "df004d"
                , "cf004d"
                , "bf004d"
                ]


-- http://www.color-hex.com/color-palette/42891
fairyFloss :: Colours
fairyFloss = toC [ "c660ff"
                 , "f69dea"
                 , "ffd164"
                 , "9270ff"
                 , "5c0000"
                 ]


-- http://www.color-hex.com/color-palette/42877
oldTrellisPayne :: Colours
oldTrellisPayne = toC [ "ffa077"
                      , "f2b8b8"
                      , "87e1cd"
                      , "87a986"
                      , "211f6d"
                      ]


-- http://www.color-hex.com/color-palette/42827
spaceShock :: Colours
spaceShock = toC [ "000147"
                 , "340060"
                 , "370056"
                 , "4e0058"
                 , "8b009c"
                 ]


-- http://www.color-hex.com/color-palette/42807
wald :: Colours
wald = toC [ "ffea6d"
           , "f9a872"
           , "837dab"
           , "464fc5"
           , "34309a"
           ]


-- http://www.color-hex.com/color-palette/42793
gothamAtSunrise :: Colours
gothamAtSunrise = toC [ "161616"
                      , "dee5ef"
                      , "ffe177"
                      , "ff9523"
                      , "ff7795"
                      ]


-- http://www.color-hex.com/color-palette/42786
dashboardInterns :: Colours
dashboardInterns = toC [ "d41243"
                       , "ffe000"
                       , "2ce386"
                       , "409da5"
                       , "cbb5db"
                       ]


-- http://www.color-hex.com/color-palette/42758
anythingBut :: Colours
anythingBut = toC [ "ff65d9"
                  , "ff5151"
                  , "a375ff"
                  , "ff38a7"
                  , "ff9d67"
                  ]


-- http://www.color-hex.com/color-palette/42745
crystalReef :: Colours
crystalReef = toC [ "7effc8"
                  , "ddfba8"
                  , "eae081"
                  , "8769d0"
                  , "ee8696"
                  ]


-- http://www.color-hex.com/color-palette/42735
asmShirt :: Colours
asmShirt = toC [ "ffffff"
               , "6c0fae"
               , "1b9dff"
               , "ffe759"
               , "ffd44e"
               ]


-- http://www.color-hex.com/color-palette/42711 
nightLagoon :: Colours
nightLagoon = toC [ "13b094"
                  , "2859df"
                  , "1d1f9a"
                  , "8b21e5"
                  , "9298ea"
                  ]


-- http://www.color-hex.com/color-palette/42669
projectAI :: Colours
projectAI = toC [ "ffef4d"
                , "2b2913"
                , "3a3828"
                , "908e7a"
                , "9dd1d2"
                ]


-- http://www.color-hex.com/color-palette/42652
pinkLadies :: Colours
pinkLadies = toC [ "d61978"
                 , "ff3e3e"
                 , "ff308e"
                 , "ff8d18"
                 , "ffec34"
                 ]


-- http://www.color-hex.com/color-palette/42636
ifSheHadABeach :: Colours
ifSheHadABeach = toC [ "554468"
                     , "db4264"
                     , "ef6389"
                     , "ff92b5"
                     , "fee398"
                     ]


-- http://www.color-hex.com/color-palette/42634
toFlyAway :: Colours
toFlyAway = toC [ "595fed"
                , "487de4"
                , "5abbf1"
                , "71d1d7"
                , "a1e4d3"
                ]


-- http://www.color-hex.com/color-palette/42587
sexDayDreams :: Colours
sexDayDreams = toC [ "ffb8e9"
                   , "efc8d9"
                   , "dfd8c9"
                   , "cfe8b9"
                   , "bff8a9"
                   ]


-- http://www.color-hex.com/color-palette/42488
dreamyClouds :: Colours
dreamyClouds = toC [ "cc4faa"
                   , "da3c84"
                   , "f03782"
                   , "f7a481"
                   , "fee8a3"
                   ]


-- http://www.color-hex.com/color-palette/42389
singingInTheEclipse :: Colours
singingInTheEclipse = toC [ "5b4ee7"
                          , "9c3eff"
                          , "da35ee"
                          , "f41d74"
                          , "e32121"
                          ]


 -- http://www.color-hex.com/color-palette/42390
goldfish :: Colours
goldfish = toC [ "ffae49"
               , "ffc391"
               , "8fd4ff"
               , "63b4ff"
               , "d6d6d6"
               ]


-- http://www.color-hex.com/color-palette/42342
sunsetWonder :: Colours
sunsetWonder = toC [ "ff2e87"
                   , "ff4c78"
                   , "ff6161"
                   , "ffa578"
                   , "ffd684"
                   ]


-- http://www.color-hex.com/color-palette/42105
dreamViolet :: Colours
dreamViolet = toC [ "c185f9"
                  , "af73f3"
                  , "9966f1"
                  , "8846f9"
                  , "734ff5"
                  ]


-- http://www.color-hex.com/color-palette/42079
nonmeaningful :: Colours
nonmeaningful = toC [ "ff0081"
                    , "7c00ff"
                    , "ffdb00"
                    , "ff82ed"
                    , "ffe1f5"
                    ]


-- http://www.color-hex.com/color-palette/41773
proudMountains :: Colours
proudMountains = toC [ "328b41"
                     , "009a60"
                     , "3c4b3d"
                     , "363a38"
                     , "21396b"
                     ]


-- http://www.color-hex.com/color-palette/41778
pulpy :: Colours
pulpy = toC [ "fd00ff"
            , "f000ff"
            , "a300ff"
            , "009fff"
            , "6300ff"
            ]


-- http://www.color-hex.com/color-palette/43681
girlNextDoor :: Colours
girlNextDoor = toC [ "b05dff"
                   , "fad4ff"
                   , "ffa6ca"
                   , "a68ab6"
                   , "7a8fa7"
                   ]


-- http://www.color-hex.com/color-palette/43692
washfastAcid :: Colours
washfastAcid = toC [ "ffff01"
                   , "ffb401"
                   , "ee0000"
                   , "da0074"
                   , "5f009c"
                   ]


-- http://www.color-hex.com/color-palette/43640
waterAndTheWaves :: Colours
waterAndTheWaves = toC [ "000d3c"
                       , "003276"
                       , "0046bd"
                       , "00a3ba"
                       , "00e6f9"
                       ]


-- http://www.color-hex.com/color-palette/43604
violetEnergy :: Colours
violetEnergy = toC [ "c770e5"
                   , "9e65db"
                   , "6150d0"
                   , "4a299c"
                   , "390985"
                   ]


-- http://www.color-hex.com/color-palette/43571
smoothScaling :: Colours
smoothScaling = toC [ "f9eead"
                    , "d5f9b6"
                    , "bff8cb"
                    , "c7f7f2"
                    , "cfe0f6"
                    ]


-- http://www.color-hex.com/color-palette/43561
logga :: Colours
logga = toC [ "e72d2d"
            , "4b0916"
            , "9f1b33"
            , "ff663f"
            , "00aebd"
            ]


-- http://www.color-hex.com/color-palette/43521
noodles :: Colours
noodles = toC [ "f9e2cd"
              , "9c6fad"
              , "9dcbfb"
              , "71e6cc"
              , "ff5e8c"
              ]


-- http://www.color-hex.com/color-palette/43313
frenchFlag :: Colours
frenchFlag = toC [ "ff0000"
                 , "cf0581"
                 , "0015fd"
                 , "7900ff"
                 , "ffffff"
                 ]


-- http://www.color-hex.com/color-palette/43281
lire :: Colours
lire = toC [ "12a16b"
           , "f09090"
           , "0064a3"
           , "fdae33"
           , "bf78b0"
           ]


-- http://www.color-hex.com/color-palette/43263
pinkCrystals :: Colours
pinkCrystals = toC [ "faa0ff"
                   , "e59efe"
                   , "bd9bf1"
                   , "837eda"
                   , "6c55ee"
                   ]


-- http://www.color-hex.com/color-palette/43265
aquaCrystals :: Colours
aquaCrystals = toC [ "8db4f9"
                   , "82bfff"
                   , "82d5fd"
                   , "57fbfa"
                   , "31f3ff"
                   ]


-- http://www.color-hex.com/color-palette/43240
orangeCherry :: Colours
orangeCherry = toC [ "f4543b"
                   , "f4443b"
                   , "f4343b"
                   , "f4243b"
                   , "f4143b"
                   ]


-- http://www.color-hex.com/color-palette/43242
plumToCerulean :: Colours
plumToCerulean = toC [ "e8a0e8"
                     , "d0a0e8"
                     , "b8a0e8"
                     , "a0a0e8"
                     , "a0b8e8"
                     ]


-- http://www.color-hex.com/color-palette/43238
sunsetAtSea :: Colours
sunsetAtSea = toC [ "b5e8d2"
                  , "c9e8d2"
                  , "dee7d0"
                  , "f1e8d2"
                  , "ffe3ce"
                  ]


-- http://www.color-hex.com/color-palette/43157
cleanAndEnergetic :: Colours
cleanAndEnergetic = toC [ "5680e9"
                        , "84ceeb"
                        , "5ab9ea"
                        , "c1c8e4"
                        , "8860d0"
                        ]


-- http://www.color-hex.com/color-palette/43082
lightPastel :: Colours
lightPastel = toC [ "ece0f4"
                  , "efe5ff"
                  , "ffe9f4"
                  , "ffeeea"
                  , "fafbea"
                  ]


-- http://www.color-hex.com/color-palette/43073
everyDayIsNight :: Colours
everyDayIsNight = toC [ "ff4386"
                      , "ff2ec1"
                      , "1d1074"
                      , "c3008a"
                      , "63004b"
                      ]
