module BCP47
    exposing
        ( Extension
        , Grandfathered
            ( Irregular
            , Regular
            )
        , Irregular
            ( EnGBOed
            , IAmi
            , IBnn
            , IDefault
            , IEnochian
            , IHak
            , IKlingon
            , ILux
            , IMingo
            , INavajo
            , IPwn
            , ITao
            , ITay
            , ITsu
            , SgnBEFR
            , SgnBENL
            , SgnCHDE
            )
        , Language
        , LanguageExtension
        , LanguageTag
            ( GrandfatheredLanguageTag
            , LanguageTag
            , PrivateUse
            )
        , LanguageTagData
        , Region
            ( ISO3166_1
            , UN_M49
            )
        , Regular
            ( ArgLojban
            , CelGaulish
            , NoBok
            , NoNyn
            , ZhGuoyu
            , ZhHakka
            , ZhMin
            , ZhMinNan
            , ZhXiang
            )
        , parser
        )

{-| Parser language tags according to the [BCP
47](https://tools.ietf.org/html/rfc5646) specifications.

@docs parser

@docs LanguageTag, LanguageTagData, Language, LanguageExtension, Region, Extension, Grandfathered, Irregular, Regular

-}

import Char
import Parser exposing (..)


{-| -}
type LanguageTag
    = LanguageTag LanguageTagData
    | PrivateUse (List String)
    | GrandfatheredLanguageTag Grandfathered


{-| -}
type alias LanguageTagData =
    { language : Language
    , script : Maybe String
    , region : Maybe Region
    , variants : List String
    , extensions : List Extension
    , privateUse : List String
    }


{-| -}
type alias Language =
    { shortestIso639Code : String
    , extension : Maybe LanguageExtension
    }


{-| -}
type alias LanguageExtension =
    { selectedIso639Code : String
    , reserved : List String
    }


{-| -}
type Region
    = ISO3166_1 String
    | UN_M49 Int


{-| -}
type alias Extension =
    { kind : String
    , values : List String
    }


{-| -}
type Grandfathered
    = Irregular Irregular
    | Regular Regular


{-| -}
type Irregular
    = EnGBOed
    | IAmi
    | IBnn
    | IDefault
    | IEnochian
    | IHak
    | IKlingon
    | ILux
    | IMingo
    | INavajo
    | IPwn
    | ITao
    | ITay
    | ITsu
    | SgnBEFR
    | SgnBENL
    | SgnCHDE


{-| -}
type Regular
    = ArgLojban
    | CelGaulish
    | NoBok
    | NoNyn
    | ZhGuoyu
    | ZhHakka
    | ZhMin
    | ZhMinNan
    | ZhXiang



---- PARSER


{-| -}
parser : Parser LanguageTag
parser =
    oneOf
        [ map PrivateUse privateUseParser
        , map LanguageTag languageTagDataParser

        --, grandfatheredParser
        ]


languageTagDataParser : Parser LanguageTagData
languageTagDataParser =
    succeed LanguageTagData
        |= languageParser
        |= oneOf
            [ succeed Just
                |= untilEndOrMinus (alpha 4)
            , succeed Nothing
            ]
        |= oneOf
            [ succeed (Just << ISO3166_1)
                |= untilEndOrMinus (alpha 2)
            , succeed (Just << UN_M49)
                |= untilEndOrMinus int
            , succeed Nothing
            ]
        |= variantsParser
        |= extensionsParser
        |= oneOf
            [ privateUseParser
            , succeed []
            ]


languageParser : Parser Language
languageParser =
    succeed Language
        |= alphaRange 2 3
        |. endOrMinus
        |= oneOf
            [ delayedCommitMap (\languageExtension _ -> languageExtension)
                (succeed Just
                    |= languageExtensionParser
                )
                (succeed ())
            , succeed Nothing
            ]


languageExtensionParser : Parser LanguageExtension
languageExtensionParser =
    succeed LanguageExtension
        |= untilEndOrMinus (alpha 3)
        |= succeed []


variantsParser : Parser (List String)
variantsParser =
    variantsParserHelp []


variantsParserHelp : List String -> Parser (List String)
variantsParserHelp variants =
    oneOf
        [ nextVariant
            |> andThen (\variant -> variantsParserHelp (variant :: variants))
        , succeed (List.reverse variants)
        ]


nextVariant : Parser String
nextVariant =
    delayedCommitMap (\variant _ -> variant)
        (succeed identity
            |= oneOf
                [ succeed identity
                    |= untilEndOrMinus (alphaRange 5 8)
                , untilEndOrMinus
                    (succeed (++)
                        |= digit 1
                        |= alphaDigit 3
                    )
                ]
        )
        (succeed ())


extensionsParser : Parser (List Extension)
extensionsParser =
    extensionsParserHelp []


extensionsParserHelp : List Extension -> Parser (List Extension)
extensionsParserHelp extensions =
    oneOf
        [ nextExtension
            |> andThen (\extension -> extensionsParserHelp (extension :: extensions))
        , succeed (List.reverse extensions)
        ]


nextExtension : Parser Extension
nextExtension =
    delayedCommitMap (\extension _ -> extension)
        (succeed Extension
            |= source
                (ignore (Exactly 1)
                    (\char ->
                        (char /= 'x')
                            && (char /= 'X')
                            && (isAlpha char || Char.isDigit char)
                    )
                )
            |. endOrMinus
            |= valuesParser
        )
        (succeed ())


valuesParser : Parser (List String)
valuesParser =
    valueParser
        |> andThen (\value -> valuesParserHelp [ value ])


valuesParserHelp : List String -> Parser (List String)
valuesParserHelp values =
    oneOf
        [ valueParser
            |> andThen (\value -> valuesParserHelp (value :: values))
        , succeed (List.reverse values)
        ]


valueParser : Parser String
valueParser =
    untilEndOrMinus (alphaRange 2 8)


privateUseParser : Parser (List String)
privateUseParser =
    delayedCommit
        (oneOf
            [ symbol "x"
            , symbol "X"
            ]
            |. endOrMinus
        )
        privateUseValuesParser


privateUseValuesParser : Parser (List String)
privateUseValuesParser =
    privateUseValueParser
        |> andThen (\value -> privateUseValuesParserHelp [ value ])


privateUseValuesParserHelp : List String -> Parser (List String)
privateUseValuesParserHelp values =
    oneOf
        [ privateUseValueParser
            |> andThen (\value -> privateUseValuesParserHelp (value :: values))
        , succeed (List.reverse values)
        ]


privateUseValueParser : Parser String
privateUseValueParser =
    untilEndOrMinus (alphaRange 1 8)



---- HELPER


alpha : Int -> Parser String
alpha count =
    source <|
        ignore (Exactly count) isAlpha


alphaRange : Int -> Int -> Parser String
alphaRange min max =
    source
        (ignore (Exactly min) isAlpha
            |> andThen (\_ -> nextAlpha (max - min))
        )


nextAlpha : Int -> Parser ()
nextAlpha max =
    if max > 0 then
        oneOf
            [ ignore (Exactly 1) isAlpha
                |> andThen (\_ -> nextAlpha (max - 1))
            , succeed ()
            ]
    else
        succeed ()


digit : Int -> Parser String
digit count =
    source <|
        ignore (Exactly count) Char.isDigit


alphaDigit : Int -> Parser String
alphaDigit count =
    source <|
        ignore (Exactly count) (\char -> Char.isDigit char || isAlpha char)


isAlpha : Char -> Bool
isAlpha char =
    Char.isUpper char || Char.isLower char


untilEndOrMinus : Parser a -> Parser a
untilEndOrMinus parser =
    delayedCommitMap (\value _ -> value)
        parser
        endOrMinus


endOrMinus : Parser ()
endOrMinus =
    oneOf
        [ symbol "-"
        , end
        ]
