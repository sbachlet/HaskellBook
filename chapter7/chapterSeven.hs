module Chapter7 where
    isItTwo :: Integer -> Bool
    isItTwo 2 = True
    isItTwo _ = False

    data WherePenguinsLive =
        Galapagos
      | Antarctica
      | Australia
      | SouthAfrica
      | SouthAmerica
      deriving (Eq, Show)

    data Penguin =
        Peng WherePenguinsLive
        deriving (Eq, Show)

    isSouthAfrica :: WherePenguinsLive -> Bool
    isSouthAfrica SouthAfrica  = True
    isSouthAfrica Galapagos    = False
    isSouthAfrica Antarctica    = False
    isSouthAfrica Australia    = False
    isSouthAfrica SouthAmerica = False


    isSouthAfrica' :: WherePenguinsLive -> Bool
    isSouthAfrica' SouthAfrica = True
    isSouthAfrica' _           = False

    gimmeWhereTheyLive :: Penguin
                       -> WherePenguinsLive
    gimmeWhereTheyLive (Peng whereitlives) =
        whereitlives

    humboldt :: Penguin
    gentoo :: Penguin
    macaroni :: Penguin
    little :: Penguin
    galapagos :: Penguin

    humboldt = Peng SouthAmerica
    gentoo = Peng Antarctica
    macaroni = Peng Antarctica
    little = Peng Australia
    galapagos = Peng Galapagos

    galapagosPenguin :: Penguin -> Bool
    galapagosPenguin (Peng Galapagos) = True
    galapagosPenguin _                = False

    antarcticPenguin :: Penguin -> Bool
    antarcticPenguin (Peng Antarctica) = True
    antarcticPenguin _                = False

    antarcticOrGalapagos :: Penguin -> Bool
    antarcticOrGalapagos p =
        (galapagosPenguin p)
     || (antarcticPenguin p)

    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f (a, b) (c, d) = ((b, d), (a, c))
