module Main exposing (main)

import Browser
import Html exposing (Html, b, button, div, input, node, span, text, u)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    String


type alias Msg =
    String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = \msg _ -> msg
        }


init : Model
init =
    String.fromFloat <| sqrt 2


view : Model -> Html Msg
view model =
    div [ style "padding" spacing ]
        [ styleNode
        , input [ value model, onInput identity, style "width" "100%" ] []
        , inputs
        , grids model
        ]


styleNode : Html String
styleNode =
    node "style"
        []
        [ text """
html, body {
box-sizing: border-box;
margin: 0;
padding: 0;
}

*, *:before, *:after {
box-sizing: inherit;
}"""
        ]


inputs : Html String
inputs =
    [ ( "e", e )
    , ( "π", pi )
    , ( "√2", sqrt 2 )
    , ( "φ", 0.5 + sqrt 5 / 2 )
    , let
        big =
            maxIterations + 10
      in
      ( "1/" ++ String.fromInt big, 1 / toFloat big )
    , ( "7907/7919", 7907 / 7919 )
    ]
        |> List.map
            (\( l, c ) ->
                button
                    [ onClick <| String.fromFloat c
                    , style "cursor" "pointer"
                    ]
                    [ text <| "Use " ++ l ]
            )
        |> List.intersperse (text " ")
        |> div
            [ style "margin-top" spacing
            , style "width" "100%"
            ]


grids : Model -> Html Msg
grids model =
    case String.toFloat model of
        Nothing ->
            text "Input is not a valid float"

        Just target ->
            div [ style "width" "100%" ]
                [ grid target 4 biSection
                , grid target 5 strangeSection
                ]


grid : Float -> Int -> (Float -> List ( Frac, Frac )) -> Html msg
grid target cols f =
    let
        data =
            f target

        len =
            List.length data

        formatPoint i ( lower, upper ) =
            let
                lowerDelta =
                    target - fracToFloat lower

                upperDelta =
                    fracToFloat upper - target

                highlightIf cond =
                    if cond then
                        u []

                    else
                        span []

                delta =
                    min lowerDelta upperDelta
            in
            [ b [] [ text <| String.fromInt (i + 1) ]
            , highlightIf (lowerDelta <= upperDelta) [ text <| fracToString lower ]
            , highlightIf (lowerDelta >= upperDelta) [ text <| fracToString upper ]
            , span [] [ text <| compact <| String.fromFloat delta ]
            ]

        firsts =
            data
                |> List.take 100
                |> List.indexedMap formatPoint

        lasts =
            data
                |> List.reverse
                |> List.take (min 100 (len - 100))
                |> List.reverse
                |> List.indexedMap (\i -> formatPoint (len - 100 + i))
                |> (\l ->
                        if len > 200 then
                            List.repeat (4 * cols) (span [] [ text "..." ]) :: l

                        else
                            l
                   )
    in
    (firsts ++ lasts)
        |> List.concat
        |> div
            [ style "display" "grid"
            , style "grid-template-columns" <| String.repeat cols "auto auto auto auto "
            , style "border" "1px solid black"
            , style "margin-top" spacing
            , style "column-gap" "2px"
            , style "row-gap" "2px"
            , style "width" "100%"
            ]


spacing : String
spacing =
    "20px"


compact : String -> String
compact s =
    case String.split "e" s of
        [] ->
            ""

        [ _ ] ->
            if String.startsWith "0." s then
                let
                    go z =
                        if String.startsWith ("0." ++ String.repeat z "0") s then
                            go (z + 1)

                        else
                            String.slice (z + 1) (z + 2) s ++ "e-" ++ String.fromInt z
                in
                go 1

            else
                s
                    |> String.split "."
                    |> List.head
                    |> Maybe.withDefault s

        [ b, a ] ->
            String.left 1 b ++ "e" ++ a

        _ ->
            s


fracToString : Frac -> String
fracToString ( n, d ) =
    String.fromInt n ++ "/" ++ String.fromInt d


maxIterations : Int
maxIterations =
    1000 * 1000


strangeSection : Float -> List ( Frac, Frac )
strangeSection =
    iterate <|
        \( lowern, lowerd ) ( uppern, upperd ) ->
            ( lowern + uppern, lowerd + upperd )


biSection : Float -> List ( Frac, Frac )
biSection =
    iterate <|
        \( lowern, lowerd ) ( uppern, upperd ) ->
            ( lowern * upperd + uppern * lowerd, lowerd * upperd * 2 )


type alias Frac =
    ( Int, Int )


iterate : (Frac -> Frac -> Frac) -> Float -> List ( Frac, Frac )
iterate f target =
    let
        go i lower upper acc =
            let
                (( midn, midd ) as mid) =
                    simplify <| f lower upper

                safe =
                    2 ^ 52
            in
            if
                (midd > safe)
                    || (midd < 0)
                    || (midn > safe)
                    || (midn < 0)
                    || (fracToFloat lower == target)
                    || (fracToFloat upper == target)
                    || (i > maxIterations)
            then
                List.reverse acc

            else
                let
                    ( lower_, upper_, next ) =
                        if fracToFloat mid < target then
                            ( mid, upper, ( mid, upper ) )

                        else
                            ( lower, mid, ( lower, mid ) )
                in
                go (i + 1) lower_ upper_ (next :: acc)
    in
    go 1 ( floor target, 1 ) ( ceiling target, 1 ) []


simplify : Frac -> Frac
simplify ( n, d ) =
    let
        g =
            gcd n d
    in
    ( n // g, d // g )


gcd : Int -> Int -> Int
gcd l r =
    if l < 0 then
        -(gcd -l r)

    else if r < 0 then
        -(gcd l -r)

    else if l > r then
        gcd r l

    else if l == 0 then
        r

    else
        gcd (modBy l r) l


fracToFloat : Frac -> Float
fracToFloat ( n, d ) =
    toFloat n / toFloat d
