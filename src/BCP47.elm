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
            ( Grandfathered
            , LanguageTag
            , PrivateUse
            )
        , LanguageTagData
        , Region
            ( ISO3166_1
            , UN_M49
            )
        , Regular
            ( ArtLojban
            , CelGaulish
            , NoBok
            , NoNyn
            , ZhGuoyu
            , ZhHakka
            , ZhMin
            , ZhMinNan
            , ZhXiang
            )
        , asString
        , fromString
        )

{-| Parser language tags according to the [BCP
47](https://tools.ietf.org/html/rfc5646) specifications.

@docs LanguageTag, LanguageTagData, Language, LanguageExtension, Region, Extension, Grandfathered, Irregular, Regular

@docs fromString, asString

-}

import Char
import Parser exposing (..)


{-| This type models a language tag according to the specification in [BCP
47](https://tools.ietf.org/html/rfc5646).
-}
type LanguageTag
    = LanguageTag LanguageTagData
    | PrivateUse (List String)
    | Grandfathered Grandfathered


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
    = ArtLojban
    | CelGaulish
    | NoBok
    | NoNyn
    | ZhGuoyu
    | ZhHakka
    | ZhMin
    | ZhMinNan
    | ZhXiang


{-| Convert a `LanguageTag` into its canonical string representation.
-}
asString : LanguageTag -> String
asString languageTag =
    case canonicalize languageTag of
        LanguageTag languageTagData ->
            [ Just languageTagData.language.shortestIso639Code
            , Maybe.map .selectedIso639Code languageTagData.language.extension
            , languageTagData.language.extension
                |> Maybe.andThen
                    (\extension ->
                        if List.isEmpty extension.reserved then
                            Nothing
                        else
                            extension.reserved
                                |> String.join "-"
                                |> Just
                    )
            , languageTagData.script
            , languageTagData.region
                |> Maybe.map
                    (\region ->
                        case region of
                            ISO3166_1 text ->
                                text

                            UN_M49 digit ->
                                toString digit
                    )
            , if List.isEmpty languageTagData.variants then
                Nothing
              else
                languageTagData.variants
                    |> String.join "-"
                    |> Just
            , if List.isEmpty languageTagData.extensions then
                Nothing
              else
                languageTagData.extensions
                    |> List.map
                        (\extension ->
                            [ extension.kind
                            , extension.values
                                |> String.join "-"
                            ]
                                |> String.join "-"
                        )
                    |> String.join "-"
                    |> Just
            , if List.isEmpty languageTagData.privateUse then
                Nothing
              else
                [ "x"
                , languageTagData.privateUse
                    |> String.join "-"
                ]
                    |> String.join "-"
                    |> Just
            ]
                |> List.filterMap identity
                |> String.join "-"

        PrivateUse values ->
            String.join "-" ("x" :: values)

        Grandfathered grandfathered ->
            case grandfathered of
                Irregular irregular ->
                    case irregular of
                        EnGBOed ->
                            "en-GB-oed"

                        IAmi ->
                            "i-ami"

                        IBnn ->
                            "i-bnn"

                        IDefault ->
                            "i-default"

                        IEnochian ->
                            "i-enochian"

                        IHak ->
                            "i-hak"

                        IKlingon ->
                            "i-klingon"

                        ILux ->
                            "i-lux"

                        IMingo ->
                            "i-mingo"

                        INavajo ->
                            "i-navajo"

                        IPwn ->
                            "i-pwn"

                        ITao ->
                            "i-tao"

                        ITay ->
                            "i-tay"

                        ITsu ->
                            "i-tsu"

                        SgnBEFR ->
                            "sgn-BE-FR"

                        SgnBENL ->
                            "sgn-BE-NL"

                        SgnCHDE ->
                            "sgn-CH-DE"

                Regular regular ->
                    case regular of
                        ArtLojban ->
                            "arg-lojban"

                        CelGaulish ->
                            "cel-gaulish"

                        NoBok ->
                            "no-bok"

                        NoNyn ->
                            "no-nyn"

                        ZhGuoyu ->
                            "zh-guoyu"

                        ZhHakka ->
                            "zh-hakka"

                        ZhMin ->
                            "zh-min"

                        ZhMinNan ->
                            "zh-min-nan"

                        ZhXiang ->
                            "zh-xiang"


canonicalize : LanguageTag -> LanguageTag
canonicalize languageTag =
    case languageTag of
        LanguageTag ({ language } as languageTagData) ->
            LanguageTag
                { language =
                    { shortestIso639Code =
                        String.toLower language.shortestIso639Code
                    , extension =
                        Maybe.map
                            (\languageExtension ->
                                { selectedIso639Code =
                                    String.toLower languageExtension.selectedIso639Code
                                , reserved =
                                    List.map String.toLower languageExtension.reserved
                                }
                            )
                            language.extension
                    }
                , script = Maybe.map capitalize languageTagData.script
                , region =
                    Maybe.map
                        (\region ->
                            case region of
                                ISO3166_1 text ->
                                    ISO3166_1 (String.toUpper text)

                                UN_M49 _ ->
                                    region
                        )
                        languageTagData.region
                , variants = List.map (canonicalizeSubTag 1) languageTagData.variants
                , extensions =
                    List.map
                        (\extension ->
                            { kind = String.toLower extension.kind
                            , values = List.indexedMap canonicalizeSubTag extension.values
                            }
                        )
                        languageTagData.extensions
                , privateUse = List.indexedMap canonicalizeSubTag languageTagData.privateUse
                }

        PrivateUse values ->
            PrivateUse (List.indexedMap canonicalizeSubTag values)

        Grandfathered _ ->
            languageTag


capitalize : String -> String
capitalize text =
    case String.toList text of
        first :: rest ->
            String.cons
                (Char.toUpper first)
                (rest
                    |> String.fromList
                    |> String.toLower
                )

        [] ->
            ""


canonicalizeSubTag : Int -> String -> String
canonicalizeSubTag index text =
    if index /= 0 then
        case String.length text of
            2 ->
                String.toUpper text

            4 ->
                capitalize text

            _ ->
                String.toLower text
    else
        String.toLower text



---- PARSER


{-| Attempt to read a language tag from a string.
-}
fromString : String -> Result Parser.Error LanguageTag
fromString =
    String.toLower
        >> Parser.run parser
        >> Result.map canonicalize


parser : Parser LanguageTag
parser =
    oneOf
        [ map Grandfathered grandfatheredParser
        , map PrivateUse privateUseParser
        , map LanguageTag languageTagDataParser
        ]



-- LANGUAGE TAG DATA


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



-- PRIVATE USE


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



-- GRANDFATHERED


grandfatheredParser : Parser Grandfathered
grandfatheredParser =
    oneOf
        [ map Irregular irregularParser
        , map Regular regularParser
        ]


irregularParser : Parser Irregular
irregularParser =
    oneOf
        [ succeed EnGBOed |. keyword "en-gb-oed"
        , succeed IAmi |. keyword "i-ami"
        , succeed IBnn |. keyword "i-bnn"
        , succeed IDefault |. keyword "i-default"
        , succeed IEnochian |. keyword "i-enochian"
        , succeed IHak |. keyword "i-hak"
        , succeed IKlingon |. keyword "i-klingon"
        , succeed ILux |. keyword "i-lux"
        , succeed IMingo |. keyword "i-mingo"
        , succeed INavajo |. keyword "i-navajo"
        , succeed IPwn |. keyword "i-pwn"
        , succeed ITao |. keyword "i-tao"
        , succeed ITay |. keyword "i-tay"
        , succeed ITsu |. keyword "i-tsu"
        , succeed SgnBEFR |. keyword "sgn-be-fr"
        , succeed SgnBENL |. keyword "sgn-be-nl"
        , succeed SgnCHDE |. keyword "sgn-ch-de"
        ]


regularParser : Parser Regular
regularParser =
    oneOf
        [ succeed ArtLojban |. keyword "art-lojban"
        , succeed CelGaulish |. keyword "cel-gaulish"
        , succeed NoBok |. keyword "no-bok"
        , succeed NoNyn |. keyword "no-nyn"
        , succeed ZhGuoyu |. keyword "zh-guoyu"
        , succeed ZhHakka |. keyword "zh-hakka"
        , succeed ZhMin |. keyword "zh-min"
        , succeed ZhMinNan |. keyword "zh-min-nan"
        , succeed ZhXiang |. keyword "zh-xiang"
        ]



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
