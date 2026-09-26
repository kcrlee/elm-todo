port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, column, el, fill, maximum, padding, paddingXY, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html.Events
import Json.Decode as D
import Url



-- MAIN


main : Program () Model Msg
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


{-| A todo as stored in Convex. The id comes from Convex's `_id` field.
-}
type alias Todo =
    { id : String
    , title : String
    , description : String
    , isCompleted : Bool
    }


{-| A todo that hasn't been created yet, so it has no id.
-}
type alias Draft =
    { title : String
    , description : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , todos : List Todo
    , draft : Draft
    }


emptyDraft : Draft
emptyDraft =
    { title = "", description = "" }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, todos = [], draft = emptyDraft }, Cmd.none )



-- PORTS


{-| Elm → JS: run the `todos:create` mutation.
-}
port createTodo : Draft -> Cmd msg


{-| Elm → JS: run the `todos:markCompleted` mutation. Field names match its args.
-}
port markCompleted : { todoId : String, isCompleted : Bool } -> Cmd msg


{-| JS → Elm: the latest result of the `todos:list` query subscription.
-}
port todosUpdated : (D.Value -> msg) -> Sub msg


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map4 Todo
        (D.field "_id" D.string)
        (D.field "title" D.string)
        (D.field "description" D.string)
        (D.field "isCompleted" D.bool)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TitleChanged String
    | DescriptionChanged String
    | AddTodo
    | CompletedToggled String Bool
    | TodosUpdated D.Value


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

        TitleChanged title ->
            let
                draft =
                    model.draft
            in
            ( { model | draft = { draft | title = title } }, Cmd.none )

        DescriptionChanged description ->
            let
                draft =
                    model.draft
            in
            ( { model | draft = { draft | description = description } }, Cmd.none )

        AddTodo ->
            let
                title =
                    String.trim model.draft.title
            in
            if String.isEmpty title then
                ( model, Cmd.none )

            else
                -- Convex is the source of truth: the new todo arrives back
                -- through todosUpdated, so only the draft is reset here.
                ( { model | draft = emptyDraft }
                , createTodo { title = title, description = String.trim model.draft.description }
                )

        CompletedToggled todoId isCompleted ->
            let
                setCompleted todo =
                    if todo.id == todoId then
                        { todo | isCompleted = isCompleted }

                    else
                        todo
            in
            -- Optimistic: update locally so the checkbox and model agree right
            -- away; the next todosUpdated replaces this with Convex's result.
            ( { model | todos = List.map setCompleted model.todos }
            , markCompleted { todoId = todoId, isCompleted = isCompleted }
            )

        TodosUpdated value ->
            case D.decodeValue (D.list todoDecoder) value of
                Ok todos ->
                    ( { model | todos = todos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    todosUpdated TodosUpdated



-- VIEW


colors :
    { page : Element.Color
    , card : Element.Color
    , border : Element.Color
    , text : Element.Color
    , muted : Element.Color
    , accent : Element.Color
    , onAccent : Element.Color
    }
colors =
    { page = rgb255 244 245 247
    , card = rgb255 255 255 255
    , border = rgb255 222 225 230
    , text = rgb255 33 37 41
    , muted = rgb255 134 142 150
    , accent = rgb255 59 91 219
    , onAccent = rgb255 255 255 255
    }


{-| elm-ui has no <form>, so Enter-to-submit is wired up on each input instead.
-}
onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "not Enter"
                    )
            )
        )


textField : String -> String -> (String -> Msg) -> Element Msg
textField label value onChange =
    Input.text
        [ onEnter AddTodo
        , Border.rounded 6
        , Border.color colors.border
        ]
        { onChange = onChange
        , text = value
        , placeholder = Just (Input.placeholder [] (text label))
        , label = Input.labelHidden label
        }


viewForm : Draft -> Element Msg
viewForm draft =
    column [ width fill, spacing 8 ]
        [ textField "Title" draft.title TitleChanged
        , textField "Description (optional)" draft.description DescriptionChanged
        , Input.button
            [ alignRight
            , paddingXY 16 10
            , Border.rounded 6
            , Background.color colors.accent
            , Font.color colors.onAccent
            , Font.semiBold
            ]
            { onPress = Just AddTodo
            , label = text "Add todo"
            }
        ]


viewTodo : Todo -> ( String, Element Msg )
viewTodo todo =
    let
        titleStyle =
            if todo.isCompleted then
                [ Font.strike, Font.color colors.muted ]

            else
                [ Font.semiBold ]

        description =
            if String.isEmpty todo.description then
                Element.none

            else
                paragraph [ Font.size 14, Font.color colors.muted ] [ text todo.description ]
    in
    ( todo.id
    , Input.checkbox
        [ paddingXY 0 12
        , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
        , Border.color colors.border
        ]
        { onChange = CompletedToggled todo.id
        , icon = Input.defaultCheckbox
        , checked = todo.isCompleted
        , label =
            Input.labelRight [ width fill, Element.paddingEach { top = 0, right = 0, bottom = 0, left = 8 } ]
                (column [ width fill, spacing 4 ]
                    [ paragraph titleStyle [ text todo.title ]
                    , description
                    ]
                )
        }
    )


viewTodos : List Todo -> Element Msg
viewTodos todos =
    if List.isEmpty todos then
        el [ centerX, paddingXY 0 16, Font.color colors.muted ] (text "Nothing to do yet.")

    else
        Keyed.column [ width fill ] (List.map viewTodo todos)


view : Model -> Browser.Document Msg
view model =
    let
        remaining =
            List.length (List.filter (not << .isCompleted) model.todos)
    in
    { title = "Todo List"
    , body =
        [ Element.layout
            [ Background.color colors.page
            , Font.color colors.text
            , Font.family [ Font.typeface "Inter", Font.sansSerif ]
            , Font.size 16
            , padding 16
            ]
            (column
                [ centerX
                , width (fill |> maximum 560)
                , spacing 20
                , padding 24
                , Background.color colors.card
                , Border.rounded 10
                , Border.shadow { offset = ( 0, 1 ), size = 0, blur = 4, color = Element.rgba 0 0 0 0.08 }
                ]
                [ row [ width fill ]
                    [ el [ Font.size 28, Font.bold ] (text "Todos")
                    , el [ alignRight, Font.size 14, Font.color colors.muted ]
                        (text (String.fromInt remaining ++ " remaining"))
                    ]
                , viewForm model.draft
                , viewTodos model.todos
                ]
            )
        ]
    }
