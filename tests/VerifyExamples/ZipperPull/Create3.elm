module VerifyExamples.ZipperPull.Create3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import ZipperPull exposing (..)

type alias Model =
    { title : String
    , readCount : Int
    , previousVolumes : List (String, Int)
    , latterVolumes : List (String, Int)
    }

pull : Interface (String, Int) Model output
pull =
    create
        { getRightList = .latterVolumes
        , getLeftList = .previousVolumes
        , getFocus = \m -> (m.title, m.readCount)
        , setRightList = \l m -> {m| latterVolumes = l}
        , setLeftList = \f m -> {m| previousVolumes = f}
        , setFocus = \(t,c) m -> {m| title=t, readCount=c}
        }
model : Model
model =
    { title = "Philosopher's Stone"
    , readCount = 3
    , previousVolumes = []
    , latterVolumes =
        [ ("Chamber of Secrets",2)
        , ("Prisoner of Azkaban",2)
        , ("Goblet of Fire",3)
        , ("Order of the Phoenix",2)
        , ("Half-Blood Prince",1)
        , ("Deathly Hallows",1)
        ]
    }



spec3 : Test.Test
spec3 =
    Test.test "#create: \n\n    model |> pull.focusRightEnd |> pull.isRightEnd\n    --> True" <|
        \() ->
            Expect.equal
                (
                model |> pull.focusRightEnd |> pull.isRightEnd
                )
                (
                True
                )