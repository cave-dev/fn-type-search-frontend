module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, a, header, p, hr)
import Html.Attributes exposing (placeholder, value, class, href)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Http


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { query = "", results = Nothing }, Cmd.none )


type Msg
    = Search
    | UpdateQuery String
    | NewResult (Result Http.Error (List SearchResult))


type alias TypeSignature =
    String


type alias Model =
    { query : TypeSignature
    , results : Maybe (List SearchResult)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( model
            , Http.send NewResult
                (Http.get
                    ("http://localhost:8000/search/" ++ model.query |> String.split ("->") |> List.map (String.trim) |> String.join (" "))
                    searchResultDecoder
                )
            )

        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        NewResult result ->
            case result of
                Ok data ->
                    ( { model | results = Just data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


functionView : SearchResult -> Html Msg
functionView res =
    div [ class "card" ]
        [ header [ class "card-header" ]
            [ p [ class "card-header-title" ]
                [ text (res.func_name ++ " : " ++ (res.func_type_sig |> String.split (" ") |> List.map (String.trim) |> String.join (" -> "))) ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ text res.repo_name
                , text ". "
                , a [ href res.repo_url ]
                    [ text res.repo_name ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        results =
            case model.results of
                Just r ->
                    r |> List.map functionView

                Nothing ->
                    [ div [] [] ]
    in
        div [ class "columns" ]
            [ div [ class "column is-10 is-offset-1" ]
                [ text "Elm TypeSignature Search Engine"
                , hr [] []
                , input
                    [ placeholder "Text to reverse"
                    , value model.query
                    , class "input"
                    , onInput
                        UpdateQuery
                    ]
                    []
                , button [ onClick Search, class "button" ] [ text "Search" ]
                , hr [] []
                , div [] results
                ]
            ]


type alias SearchResultRepo =
    { name : String, url : String }


type alias SearchResultFn =
    { name : String, desc : String, args : List String }


type alias SearchResult =
    { repo_name : String, repo_url : String, func_name : String, func_type_sig : String }



-- type alias SearchResult =
-- { repo_id : Int, type_signature : String, name : String }


searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    Decode.map4 SearchResult
        (Decode.at [ "repo_name" ] Decode.string)
        (Decode.at [ "repo_url" ] Decode.string)
        (Decode.at [ "func_name" ] Decode.string)
        (Decode.at [ "func_type_sig" ] Decode.string)
        |> Decode.list


main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
