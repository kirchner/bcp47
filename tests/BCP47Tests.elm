module BCP47Tests exposing (..)

import BCP47 exposing (..)
import Expect
import Parser
import Test exposing (..)


parserTest : Test
parserTest =
    describe "parser"
        [ test "Simple language subtag" <|
            \_ ->
                "en"
                    |> Parser.run parser
                    |> Expect.equal
                        (Ok <|
                            LanguageTag
                                { language =
                                    { shortestIso639Code = "en"
                                    , extension = Nothing
                                    }
                                , script = Nothing
                                , region = Nothing
                                , variants = []
                                , extensions = []
                                , privateUse = []
                                }
                        )
        , test "Language subtag plus script subtag" <|
            \_ ->
                "zh-Hant"
                    |> Parser.run parser
                    |> Expect.equal
                        (Ok <|
                            LanguageTag
                                { language =
                                    { shortestIso639Code = "zh"
                                    , extension = Nothing
                                    }
                                , script = Just "Hant"
                                , region = Nothing
                                , variants = []
                                , extensions = []
                                , privateUse = []
                                }
                        )
        , describe "Extended language subtags and their primary language subtag counterparts"
            [ test "Chinese, Mandarin, Simplified script, as used in China" <|
                \_ ->
                    "zh-cmn-Hans-CN"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "zh"
                                        , extension =
                                            Just
                                                { selectedIso639Code = "cmn"
                                                , reserved = []
                                                }
                                        }
                                    , script = Just "Hans"
                                    , region = Just (ISO3166_1 "CN")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Mandarin Chinese, Simplified script, as used in China" <|
                \_ ->
                    "cmn-Hans-CN"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "cmn"
                                        , extension = Nothing
                                        }
                                    , script = Just "Hans"
                                    , region = Just (ISO3166_1 "CN")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Chinese, Cantonese, as used in Hong Kong SAR" <|
                \_ ->
                    "zh-yue-HK"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "zh"
                                        , extension =
                                            Just
                                                { selectedIso639Code = "yue"
                                                , reserved = []
                                                }
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "HK")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Cantonese Chinese, as used in Hong Kong SAR" <|
                \_ ->
                    "yue-HK"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "yue"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "HK")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Language-Script-Region"
            [ test "Chinese written using the Simplified script as used in mainland China" <|
                \_ ->
                    "zh-Hans-CN"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "zh"
                                        , extension = Nothing
                                        }
                                    , script = Just "Hans"
                                    , region = Just (ISO3166_1 "CN")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Serbian written using the Latin script as used in Serbia" <|
                \_ ->
                    "sr-Latn-RS"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "sr"
                                        , extension = Nothing
                                        }
                                    , script = Just "Latn"
                                    , region = Just (ISO3166_1 "RS")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Language-Variant"
            [ test "Resian dialect of Slovenian" <|
                \_ ->
                    "sl-rozaj"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "sl"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Nothing
                                    , variants = [ "rozaj" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "San Giorgio dialect of Resian dialect of Slovenian" <|
                \_ ->
                    "sl-rozaj-biske"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "sl"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Nothing
                                    , variants = [ "rozaj", "biske" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Nadiza dialect of Slovenian" <|
                \_ ->
                    "sl-nedis"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "sl"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Nothing
                                    , variants = [ "nedis" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Language-Region-Variant"
            [ test "German as used in Switzerland using the 1901 variant [orthography]" <|
                \_ ->
                    "de-CH-1901"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "de"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "CH")
                                    , variants = [ "1901" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Slovenian as used in Italy, Nadiza dialect" <|
                \_ ->
                    "sl-IT-nedis"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "sl"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "IT")
                                    , variants = [ "nedis" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Language-Script-Region-Variant"
            [ test "Eastern Armenian written in Latin script, as used in Italy" <|
                \_ ->
                    "hy-Latn-IT-arevela"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "hy"
                                        , extension = Nothing
                                        }
                                    , script = Just "Latn"
                                    , region = Just (ISO3166_1 "IT")
                                    , variants = [ "arevela" ]
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Language-Region"
            [ test "German for Germany" <|
                \_ ->
                    "de-DE"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "de"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "DE")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "English as used in the United States" <|
                \_ ->
                    "en-US"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "en"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "US")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            , test "Spanish appropriate for the Latin America and Caribbean region using the UN region code" <|
                \_ ->
                    "es-419"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "es"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (UN_M49 419)
                                    , variants = []
                                    , extensions = []
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "With extensions"
            [ test "Phonebook variant of the German sort order" <|
                \_ ->
                    "de-DE-u-co-phonebk"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "de"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "DE")
                                    , variants = []
                                    , extensions =
                                        [ { kind = "u"
                                          , values = [ "co", "phonebk" ]
                                          }
                                        ]
                                    , privateUse = []
                                    }
                            )
            , test "Use Thai digits in number formatting" <|
                \_ ->
                    "th-TH-u-nu-thai"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "th"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "TH")
                                    , variants = []
                                    , extensions =
                                        [ { kind = "u"
                                          , values = [ "nu", "thai" ]
                                          }
                                        ]
                                    , privateUse = []
                                    }
                            )
            , test "Use the Japanese calendar in date and time formatting" <|
                \_ ->
                    "ja-JP-u-ca-japanese"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "ja"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "JP")
                                    , variants = []
                                    , extensions =
                                        [ { kind = "u"
                                          , values = [ "ca", "japanese" ]
                                          }
                                        ]
                                    , privateUse = []
                                    }
                            )
            , test "British English with the Islamic (Hijri) calendar" <|
                \_ ->
                    "en-GB-u-ca-islamic"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "en"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "GB")
                                    , variants = []
                                    , extensions =
                                        [ { kind = "u"
                                          , values = [ "ca", "islamic" ]
                                          }
                                        ]
                                    , privateUse = []
                                    }
                            )
            ]
        , describe "Private use subtags"
            [ test "de-CH-x-phonebk" <|
                \_ ->
                    "de-CH-x-phonebk"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "de"
                                        , extension = Nothing
                                        }
                                    , script = Nothing
                                    , region = Just (ISO3166_1 "CH")
                                    , variants = []
                                    , extensions = []
                                    , privateUse = [ "phonebk" ]
                                    }
                            )
            , test "az-Arab-x-AZE-derbend" <|
                \_ ->
                    "az-Arab-x-AZE-derbend"
                        |> Parser.run parser
                        |> Expect.equal
                            (Ok <|
                                LanguageTag
                                    { language =
                                        { shortestIso639Code = "az"
                                        , extension = Nothing
                                        }
                                    , script = Just "Arab"
                                    , region = Nothing
                                    , variants = []
                                    , extensions = []
                                    , privateUse = [ "AZE", "derbend" ]
                                    }
                            )
            ]
        , test "Private use" <|
            \_ ->
                "x-whatever"
                    |> Parser.run parser
                    |> Expect.equal
                        (Ok <|
                            PrivateUse [ "whatever" ]
                        )
        ]
