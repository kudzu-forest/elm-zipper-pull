module ZipperPull.Stiff exposing
    ( Interface
    , create
    , Err(..)
    )

{-| This module provides a 'stiff' list zipper interface, meaning that the motion may fail. The failue results from two reasons:
1.the pull already comes at the left or right end.
2.the data conversion between focused and stored data fails.
In order to express this fact, some methods provided by interface made with `Zipper.Stiff.create` returns `Result`s.


# INTERFACE TYPE

@docs Interface


# CREATION

@docs create


# Error

@docs Err

-}

import Result exposing (Result)


{-| Represents failure reason in moving focus.
-}
type Err g
    = FailedToGetFocus g
    | BumpsLeftEnd
    | BumpsRightEnd


{-| A record containing functions for manipulating specified data type as list zipper. The type parameter `output` should not be specifide.
-}
type alias Interface g stock model output =
    { focusLeft : model -> Result (Err g) model
    , focusRight : model -> Result (Err g) model
    , focusLeftEnd : model -> Result (Err g) model
    , focusRightEnd : model -> Result (Err g) model
    , isLeftEnd : model -> Bool
    , isRightEnd : model -> Bool
    , indexFromLeft : model -> Int
    , indexFromRight : model -> Int
    , map : (stock -> stock) -> model -> Result (Err g) model
    , toLeftHeadList : model -> Result (Err g) (List stock)
    , toRightHeadList : model -> Result (Err g) (List stock)
    , foldl : (stock -> output -> output) -> output -> model -> Result (Err g) output
    , foldr : (stock -> output -> output) -> output -> model -> Result (Err g) output
    , mapToLeftHeadList : (stock -> output) -> model -> Result (Err g) (List output)
    , mapToRightHeadList : (stock -> output) -> model -> Result (Err g) (List output)
    }


{-| The way you can attach stiff list zipper interface on ordinary record.
-}
create :
    { getRightList : model -> List a
    , getLeftList : model -> List a
    , getFocus : model -> Result g a
    , setRightList : List a -> model -> model
    , setLeftList : List a -> model -> model
    , setFocus : a -> model -> model
    }
    -> Interface g a model b
create { getRightList, getLeftList, getFocus, setRightList, setLeftList, setFocus } =
    let
        focusRight =
            \model ->
                case getFocus model of
                    Err g ->
                        Err (FailedToGetFocus g)

                    Ok oldFocus ->
                        case getRightList model of
                            [] ->
                                Err BumpsRightEnd

                            newFocus :: rest ->
                                model
                                    |> setLeftList (oldFocus :: getLeftList model)
                                    |> setFocus newFocus
                                    |> setRightList rest
                                    |> Ok

        focusLeft =
            \model ->
                case getFocus model of
                    Err g ->
                        Err (FailedToGetFocus g)

                    Ok oldFocus ->
                        case getLeftList model of
                            [] ->
                                Err BumpsLeftEnd

                            newFocus :: rest ->
                                model
                                    |> setLeftList rest
                                    |> setFocus newFocus
                                    |> setRightList (oldFocus :: getRightList model)
                                    |> Ok

        focusRightEnd =
            \model ->
                case getFocus model of
                    Err g ->
                        Err (FailedToGetFocus g)

                    Ok oldFocus ->
                        let
                            whole =
                                List.foldl
                                    (\crr acc -> crr :: acc)
                                    (oldFocus :: getLeftList model)
                                    (getRightList model)
                        in
                        case whole of
                            [] ->
                                Ok model

                            rightEnd :: tail ->
                                model
                                    |> setLeftList tail
                                    |> setFocus rightEnd
                                    |> setRightList []
                                    |> Ok

        focusLeftEnd =
            \model ->
                case getFocus model of
                    Err g ->
                        Err (FailedToGetFocus g)

                    Ok oldFocus ->
                        let
                            whole =
                                List.foldl
                                    (\crr acc -> crr :: acc)
                                    (oldFocus :: getLeftList model)
                                    (getRightList model)
                        in
                        case whole of
                            [] ->
                                Ok model

                            rightEnd :: tail ->
                                model
                                    |> setLeftList tail
                                    |> setFocus rightEnd
                                    |> setRightList []
                                    |> Ok

        isRightEnd =
            \model -> List.isEmpty (getRightList model)

        isLeftEnd =
            \model -> List.isEmpty (getLeftList model)

        foldl =
            \f initial model ->
                case focusLeftEnd model of
                    Err e ->
                        Err e

                    Ok m ->
                        case getFocus m of
                            Err g ->
                                Err (FailedToGetFocus g)

                            Ok l ->
                                (l :: getRightList m)
                                    |> List.foldl f initial
                                    |> Ok

        foldr =
            \f initial model ->
                case focusRightEnd model of
                    Err e ->
                        Err e

                    Ok m ->
                        case getFocus m of
                            Err g ->
                                Err (FailedToGetFocus g)

                            Ok r ->
                                (r :: getLeftList m)
                                    |> List.foldl f initial
                                    |> Ok

        indexFromLeft =
            \model ->
                List.length (getLeftList model)

        indexFromRight =
            \model ->
                List.length (getRightList model)

        map =
            \f model ->
                case getFocus model of
                    Err g ->
                        Err (FailedToGetFocus g)

                    Ok oldFocus ->
                        model
                            |> setLeftList (List.map f (getLeftList model))
                            |> setFocus (f oldFocus)
                            |> setRightList (List.map f (getRightList model))
                            |> Ok

        toLeftHeadList =
            \model ->
                case focusLeftEnd model of
                    Err e ->
                        Err e

                    Ok m ->
                        case getFocus m of
                            Err g ->
                                Err (FailedToGetFocus g)

                            Ok l ->
                                l
                                    :: getRightList m
                                    |> Ok

        toRightHeadList =
            \model ->
                case focusRightEnd model of
                    Err e ->
                        Err e

                    Ok m ->
                        case getFocus m of
                            Err g ->
                                Err (FailedToGetFocus g)

                            Ok r ->
                                r
                                    :: getLeftList m
                                    |> Ok

        mapToLeftHeadList =
            \f model ->
                toLeftHeadList model
                    |> Result.map (List.map f)

        mapToRightHeadList =
            \f model ->
                toRightHeadList model
                    |> Result.map (List.map f)
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
