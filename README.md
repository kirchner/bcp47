# BCP 47 [![Build Status](https://travis-ci.org/kirchner/bcp47.svg?branch=master)](https://travis-ci.org/kirchner/bcp47)

This library lets you parse language tags according to the specifications in
[BCP 47](https://tools.ietf.org/html/rfc5646): strings like `"zh-cmn-Hans-CN"`,
`"en-US"`, `"ja-JP-u-ca-japanese"`, `"x-whatever"`, ... . The first one, for example, would be turned into

```elm
languageTag =
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
```

You can also turn these data back into canonicalized strings.
