module Main exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, src, style)
import Html.Styled.Events exposing (onClick)
import List exposing (indexedMap)
import Process
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { leftImageIndex : Int
    , rightImageIndex : Int
    , leftOpacity : Float
    , rightOpacity : Float
    , leftChanging : Bool
    , rightChanging : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { leftImageIndex = 0
      , rightImageIndex = 0
      , leftOpacity = 1.0
      , rightOpacity = 1.0
      , leftChanging = False
      , rightChanging = False
      }
    , Cmd.none
    )


leftImages : List String
leftImages =
    [ "5.jpg", "1.jpg", "3.jpg" ]


rightImages : List String
rightImages =
    [ "6.jpg", "2.jpg", "4.jpg" ]



-- UPDATE


type Msg
    = Tick Time.Posix
    | FadeOutLeft
    | FadeOutRight
    | FadeInLeft
    | FadeInRight
    | ChangeLeftImage
    | ChangeRightImage


transitionTime : Int
transitionTime =
    1000


leftRightDelayTime : Int
leftRightDelayTime =
    300


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.leftChanging || model.rightChanging then
                ( model, Cmd.none )

            else
                ( model
                , Cmd.batch
                    [ Task.perform (\_ -> FadeOutLeft) (Task.succeed ())
                    , Process.sleep (toFloat leftRightDelayTime)
                        |> Task.andThen (\_ -> Task.succeed ())
                        |> Task.perform (\_ -> FadeOutRight)
                    ]
                )

        FadeOutLeft ->
            ( { model | leftChanging = True }
            , Process.sleep (toFloat transitionTime)
                |> Task.andThen (\_ -> Task.succeed ())
                |> Task.perform (\_ -> ChangeLeftImage)
            )

        FadeOutRight ->
            ( { model | rightChanging = True }
            , Process.sleep (toFloat transitionTime)
                |> Task.andThen (\_ -> Task.succeed ())
                |> Task.perform (\_ -> ChangeRightImage)
            )

        ChangeLeftImage ->
            let
                nextIndex =
                    modBy (List.length leftImages) (model.leftImageIndex + 1)
            in
            ( { model | leftImageIndex = nextIndex }
            , Process.sleep 100
                |> Task.andThen (\_ -> Task.succeed ())
                |> Task.perform (\_ -> FadeInLeft)
            )

        ChangeRightImage ->
            let
                nextIndex =
                    modBy (List.length rightImages) (model.rightImageIndex + 1)
            in
            ( { model | rightImageIndex = nextIndex }
            , Process.sleep 100
                |> Task.andThen (\_ -> Task.succeed ())
                |> Task.perform (\_ -> FadeInRight)
            )

        FadeInLeft ->
            ( { model | leftChanging = False }
            , Cmd.none
            )

        FadeInRight ->
            ( { model | rightChanging = False }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 3000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "hero-container"
        , css
            [ height (px 640)
            , width (pct 100)
            , position relative
            ]
        , style "display" "flex"
        ]
        [ viewHalfHero model.leftImageIndex leftImages "left-hero"
        , viewHalfHero model.rightImageIndex rightImages "right-hero"
        , viewOverlay
        ]


viewHalfHero : Int -> List String -> String -> Html Msg
viewHalfHero imageIndex imageList className =
    let
        transition =
            "opacity " ++ String.fromInt transitionTime ++ "ms ease-in-out"
    in
    div
        [ class className
        , css
            [ width (pct 50)
            , height (pct 100)
            , position relative
            ]
        ]
        (indexedMap
            (\i item ->
                div
                    [ css
                        [ position absolute
                        , top zero
                        , bottom zero
                        , left zero
                        , right zero
                        , backgroundSize cover
                        , backgroundPosition center
                        ]
                    , style "background-image" ("url(" ++ item ++ ")")
                    , style "opacity"
                        (if i <= imageIndex then
                            "1"

                         else
                            "0"
                        )
                    , style "transition" transition
                    ]
                    []
            )
            imageList
        )


viewOverlay : Html msg
viewOverlay =
    div
        [ css
            [ position absolute
            , top zero
            , bottom zero
            , left zero
            , right zero
            , flexDirection column
            , justifyContent center
            , alignItems center
            , cursor pointer
            , backgroundColor (rgba 0 0 0 0)
            , hover
                [ backgroundColor (rgba 0 0 0 0.1)
                ]
            ]
        , style "display" "flex"
        , style "transition" "all .2s ease-in-out"
        ]
        [ h2
            [ css
                [ color (rgb 255 255 255)
                , fontSize (rem 3)
                ]
            ]
            [ text "The images are AI-generated!" ]
        , button
            [ css
                [ padding2 (rem 0.5) (rem 1)
                , borderRadius (rem 0.25)
                , color (rgb 255 255 255)
                , cursor pointer
                , border zero
                , backgroundColor (rgb 199 87 36)
                , hover
                    [ backgroundColor (rgb 179 78 32)
                    ]
                ]
            , style "transition" "all .2s ease-in-out"
            , style "box-shadow" "0 20px 10px -35px rgba(0, 0, 0, .3), 0px 4px 9px -5px rgba(0, 0, 0, .5)"
            ]
            [ text "Explore the SeriesName" ]
        ]
