module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, hr)
import Html.Attributes exposing (placeholder, value, class)
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
                    ("http://localhost:8000/search/" ++ model.query)
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


view : Model -> Html Msg
view model =
    let
        txt =
            case model.results of
                Just r ->
                    r
                        |> List.map
                            (\res ->
                                [ "repo:"
                                , res.repo.name
                                , "url:"
                                , res.repo.url
                                , "fn:"
                                , res.fn.name
                                , "desc:"
                                , res.fn.desc
                                , "args"
                                , res.fn.args |> String.join " "
                                ]
                                    |> String.join " "
                            )
                        |> String.join "\n"

                Nothing ->
                    "NONE"
    in
        div []
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
            , text txt
            ]


type alias SearchResultRepo =
    { name : String, url : String }


type alias SearchResultFn =
    { name : String, desc : String, args : List String }


type alias SearchResult =
    { repo : SearchResultRepo, fn : SearchResultFn }


searchResultDecoder : Decode.Decoder (List SearchResult)
searchResultDecoder =
    Decode.at [ "data" ]
        (Decode.map2 SearchResult
            (Decode.at [ "repo" ] repoDecoder)
            (Decode.at [ "res" ] resDecoder)
            |> Decode.list
        )


repoDecoder : Decode.Decoder SearchResultRepo
repoDecoder =
    Decode.map2 SearchResultRepo
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "url" ] Decode.string)


resDecoder : Decode.Decoder SearchResultFn
resDecoder =
    Decode.map3 SearchResultFn
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "desc" ] Decode.string)
        (Decode.at [ "args" ] (Decode.list Decode.string))


main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
