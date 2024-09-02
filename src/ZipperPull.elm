module ZipperPull exposing
    ( ZipperPull
    , create
    )

{-| This library makes it easier to attach interface as [linear zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)#Example:_Bidirectional_list_traversal "list zipper") on arbitrary data types including ordinary records. Actually, no data correction type is defined in this package: the only functions exposed here is `create`, which returns a record containing handler functions, not data itself, to treat your data like list zipper. So it is named ZipperPull, not Zipper itself.


# INTERFACE TYPE

@docs ZipperPull


# CREATION

@docs create

-}


{-| A record containing functions for manipulating specified data type as list zipper.
-}
type alias ZipperPull stock model output =
    { next : model -> model
    , prev : model -> model
    , first : model -> model
    , last : model -> model
    , isFirst : model -> Bool
    , isLast : model -> Bool
    , foldFromFirst : (stock -> output -> output) -> output -> model -> output
    , foldFromLast : (stock -> output -> output) -> output -> model -> output
    , position : model -> Int
    , toList : model -> List stock
    }


{-| The way you can attach list zipper interface on ordinary record (or any other data type).

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

    pull : ZipperPull (String, Int) Model output
    pull =
        create
            { getForeList = .latterVolumes
            , getRearList = .previousVolumes
            , getCurrent = \m -> (m.title, m.readCount)
            , setForeList = \l m -> {m| latterVolumes = l}
            , setRearList = \f m -> {m| previousVolumes = f}
            , setCurrent = \(t,c) m -> {m| title=t, readCount=c}
            }

    model
      |> pull.last
      |> pull.prev
      --> { title = "Half-Blood Prince" , readCount = 1 , previousVolumes = [ ("Order of the Phoenix", 2) , ("Goblet of Fire", 3) , ("Prisoner of Azkaban", 2) , ("Chamber of Secrets", 2) , ("Philosopher's Stone", 3) ] , latterVolumes = [ ("Deathly Hallows", 1) ] }

    model |> pull.isFirst --> True

    model |> pull.next |> pull.isFirst --> False

    model |> pull.isLast --> False

    model |> pull.last |> pull.isLast --> True

    model
        |> pull.foldFromFirst
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
        |> pull.foldFromLast
            (\(_, count) sofar -> sofar + count)
            0
            --> 14

    model
        |> pull.position
        --> 0

-}
create :
    { getForeList : model -> List a
    , getRearList : model -> List a
    , getCurrent : model -> a
    , setForeList : List a -> model -> model
    , setRearList : List a -> model -> model
    , setCurrent : a -> model -> model
    }
    -> ZipperPull a model b
create { getForeList, getRearList, getCurrent, setForeList, setRearList, setCurrent } =
    let
        next =
            \model ->
                case getForeList model of
                    [] ->
                        model

                    left :: rest ->
                        let
                            newForeList =
                                rest

                            newCurrent =
                                left

                            newRearList =
                                getCurrent model :: getRearList model
                        in
                        model
                            |> setForeList newForeList
                            |> setCurrent newCurrent
                            |> setRearList newRearList

        prev =
            \model ->
                case getRearList model of
                    [] ->
                        model

                    right :: rest ->
                        let
                            newForeList =
                                getCurrent model :: getForeList model

                            newCurrent =
                                right

                            newRearList =
                                rest
                        in
                        model
                            |> setForeList newForeList
                            |> setCurrent newCurrent
                            |> setRearList newRearList

        first =
            \model ->
                let
                    whole =
                        List.foldl
                            (\crr acc -> crr :: acc)
                            (getCurrent model :: getForeList model)
                            (getRearList model)
                in
                case whole of
                    f :: rest ->
                        model
                            |> setForeList rest
                            |> setCurrent f
                            |> setRearList []

                    [] ->
                        model

        last =
            \model ->
                let
                    whole =
                        List.foldl
                            (\crr acc -> crr :: acc)
                            (getCurrent model :: getRearList model)
                            (getForeList model)
                in
                case whole of
                    l :: rest ->
                        model
                            |> setForeList []
                            |> setCurrent l
                            |> setRearList rest

                    [] ->
                        model

        isFirst =
            \model -> List.isEmpty (getRearList model)

        isLast =
            \model -> List.isEmpty (getForeList model)

        foldFromFirst =
            \f initial model ->
                let
                    m =
                        first model
                in
                (getCurrent m :: getForeList m)
                    |> List.foldl f initial

        foldFromLast =
            \f initial model ->
                let
                    m =
                        last model
                in
                (getCurrent m :: getRearList m)
                    |> List.foldl f initial

        position =
            \model ->
                List.length (getRearList model)

        toList =
            \model ->
                let
                    f =
                        first model
                in
                getCurrent f :: getForeList f
    in
    { next = next
    , prev = prev
    , first = first
    , last = last
    , isFirst = isFirst
    , isLast = isLast
    , foldFromFirst = foldFromFirst
    , foldFromLast = foldFromLast
    , position = position
    , toList = toList
    }
