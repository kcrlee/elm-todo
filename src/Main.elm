port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as D
import Url



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , todos : List String
    , draft : String
    }


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init savedTodos url key =
    ( { key = key, url = url, todos = decodeTodos savedTodos, draft = "" }, Cmd.none )



-- PORTS


{-| Elm → JS: persist the todo list to localStorage.
-}
port saveTodos : List String -> Cmd msg


{-| JS → Elm: another tab changed the stored todos (raw JSON, or Nothing if cleared).
-}
port todosChanged : (Maybe String -> msg) -> Sub msg


{-| Parse the raw localStorage string, falling back to an empty list if it is
missing or corrupted.
-}
decodeTodos : Maybe String -> List String
decodeTodos raw =
    raw
        |> Maybe.andThen (D.decodeString (D.list D.string) >> Result.toMaybe)
        |> Maybe.withDefault []



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | DraftChanged String
    | AddTodo
    | TodosChanged (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        AddTodo ->
            let
                trimmed =
                    String.trim model.draft
            in
            if String.isEmpty trimmed then
                ( model, Cmd.none )

            else
                let
                    newTodos =
                        trimmed :: model.todos
                in
                ( { model | todos = newTodos, draft = "" }, saveTodos newTodos )

        DraftChanged changedDraft ->
            ( { model | draft = changedDraft }, Cmd.none )

        TodosChanged raw ->
            ( { model | todos = decodeTodos raw }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    todosChanged TodosChanged



-- VIEW


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Todo List"
    , body =
        [ Html.form [ onSubmit AddTodo ]
            [ input [ value model.draft, onInput DraftChanged ] []
            , button [] [ text "Add" ]
            ]
        , ul [] (List.map (\todo -> li [] [ text todo ]) model.todos)
        ]
    }
