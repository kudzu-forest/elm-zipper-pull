module ZipperPull.Smooth exposing
    ( Interface
    , create
    )

{-|


# INTERFACE TYPE

@docs Interface


# CREATION

@docs create


# Too few functions?

The interface does not have `updateFocus : (stock -> stock) -> model -> model`, `insertLeft : stock -> model -> model`, `getLeftList : model -> List stock` etc.
This is because these operation can be done simpler way with ordinary record operations. It may be confusing to have more than one way of doing one thing.
If you have any inconvinience, please tell me or make a pull request.

-}


{-| A record containing functions for manipulating specified data type as list zipper. The type parameter `output` should not be specifide.
-}
type alias Interface stock model output =
    { focusLeft : model -> model
    , focusRight : model -> model
    , focusLeftEnd : model -> model
    , focusRightEnd : model -> model
    , isLeftEnd : model -> Bool
    , isRightEnd : model -> Bool
    , indexFromLeft : model -> Int
    , indexFromRight : model -> Int
    , map : (stock -> stock) -> model -> model
    , toLeftHeadList : model -> List stock
    , toRightHeadList : model -> List stock
    , foldl : (stock -> output -> output) -> output -> model -> output
    , foldr : (stock -> output -> output) -> output -> model -> output
    , mapToLeftHeadList : (stock -> output) -> model -> List output
    , mapToRightHeadList : (stock -> output) -> model -> List output
    }


{-| The way you can attach list zipper interface on ordinary record.

    type alias Model =
        { title : String
        , readCount : Int
        , previousVolumes : List (String, Int)
        , latterVolumes : List (String, Int)
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

    model
      |> pull.focusRightEnd
      |> pull.focusLeft
      --> { title = "Half-Blood Prince" , readCount = 1 , previousVolumes = [ ("Order of the Phoenix", 2) , ("Goblet of Fire", 3) , ("Prisoner of Azkaban", 2) , ("Chamber of Secrets", 2) , ("Philosopher's Stone", 3) ] , latterVolumes = [ ("Deathly Hallows", 1) ] }

    model |> pull.isLeftEnd --> True

    model |> pull.focusRight |> pull.isLeftEnd --> False

    model |> pull.isRightEnd --> False

    model |> pull.focusRightEnd |> pull.isRightEnd --> True

    model
        |> pull.foldl
            (\(title, count) str ->
                str
                ++ "I have read \""
                ++ title
                ++ "\" "
                ++ String.fromInt count
                ++ " times. "
            )
            ""
            --> "I have read \"Philosopher's Stone\" 3 times. " ++ "I have read \"Chamber of Secrets\" 2 times. " ++ "I have read \"Prisoner of Azkaban\" 2 times. " ++ "I have read \"Goblet of Fire\" 3 times. " ++ "I have read \"Order of the Phoenix\" 2 times. " ++ "I have read \"Half-Blood Prince\" 1 times. " ++ "I have read \"Deathly Hallows\" 1 times. "

    model
        |> pull.foldr
            (\(_, count) sofar -> sofar + count)
            0
            --> 14

    model
        |> pull.indexFromLeft
        --> 0

-}
create :
    { getRightList : model -> List a
    , getLeftList : model -> List a
    , getFocus : model -> a
    , setRightList : List a -> model -> model
    , setLeftList : List a -> model -> model
    , setFocus : a -> model -> model
    }
    -> Interface a model b
create { getRightList, getLeftList, getFocus, setRightList, setLeftList, setFocus } =
    let
        focusRight =
            \model ->
                case getRightList model of
                    [] ->
                        model

                    left :: rest ->
                        let
                            newRightList =
                                rest

                            newFocus =
                                left

                            newLeftList =
                                getFocus model :: getLeftList model
                        in
                        model
                            |> setRightList newRightList
                            |> setFocus newFocus
                            |> setLeftList newLeftList

        focusLeft =
            \model ->
                case getLeftList model of
                    [] ->
                        model

                    right :: rest ->
                        let
                            newRightList =
                                getFocus model :: getRightList model

                            newFocus =
                                right

                            newLeftList =
                                rest
                        in
                        model
                            |> setRightList newRightList
                            |> setFocus newFocus
                            |> setLeftList newLeftList

        focusRightEnd =
            \model ->
                let
                    whole =
                        List.foldl
                            (\crr acc -> crr :: acc)
                            (getFocus model :: getLeftList model)
                            (getRightList model)
                in
                case whole of
                    l :: rest ->
                        model
                            |> setRightList []
                            |> setFocus l
                            |> setLeftList rest

                    [] ->
                        model

        focusLeftEnd =
            \model ->
                let
                    whole =
                        List.foldl
                            (\crr acc -> crr :: acc)
                            (getFocus model :: getRightList model)
                            (getLeftList model)
                in
                case whole of
                    f :: rest ->
                        model
                            |> setRightList rest
                            |> setFocus f
                            |> setLeftList []

                    [] ->
                        model

        isRightEnd =
            \model -> List.isEmpty (getRightList model)

        isLeftEnd =
            \model -> List.isEmpty (getLeftList model)

        foldl =
            \f initial model ->
                let
                    m =
                        focusLeftEnd model
                in
                (getFocus m :: getRightList m)
                    |> List.foldl f initial

        foldr =
            \f initial model ->
                let
                    m =
                        focusRightEnd model
                in
                (getFocus m :: getLeftList m)
                    |> List.foldl f initial

        indexFromLeft =
            \model ->
                List.length (getLeftList model)

        indexFromRight =
            \model ->
                List.length (getRightList model)

        map =
            \f model ->
                model
                    |> setFocus (f (getFocus model))
                    |> setLeftList (List.map f (getLeftList model))
                    |> setRightList (List.map f (getRightList model))

        toLeftHeadList =
            \model ->
                let
                    l =
                        focusLeftEnd model
                in
                getFocus l :: getRightList l

        toRightHeadList =
            \model ->
                let
                    r =
                        focusRightEnd model
                in
                getFocus r :: getLeftList r

        mapToLeftHeadList =
            \f model ->
                let
                    l =
                        focusLeftEnd model
                in
                List.map f
                    (getFocus l :: getRightList l)

        mapToRightHeadList =
            \f model ->
                let
                    r =
                        focusRightEnd model
                in
                List.map f
                    (getFocus r :: getLeftList r)
    in
    { focusRight = focusRight
    , focusLeft = focusLeft
    , focusLeftEnd = focusLeftEnd
    , focusRightEnd = focusRightEnd
    , isLeftEnd = isLeftEnd
    , isRightEnd = isRightEnd
    , indexFromLeft = indexFromLeft
    , indexFromRight = indexFromRight
    , map = map
    , toLeftHeadList = toLeftHeadList
    , toRightHeadList = toRightHeadList
    , foldl = foldl
    , foldr = foldr
    , mapToLeftHeadList = mapToLeftHeadList
    , mapToRightHeadList = mapToRightHeadList
    }
