module Main exposing (main)

import Html exposing (Html)
import Html.Events as Events
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import WebSocket


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.listen "ws://localhost:8081/stream" Echo



-- MODEL


type alias Model =
    { counter : Result String Int
    , echo : String
    }


counterDecoder : Decoder Int
counterDecoder =
    Decode.field "state" Decode.int


init : ( Model, Cmd Msg )
init =
    ( { counter = Err "Loading..."
      , echo = ""
      }
    , getCounter
    )



-- Update


type Msg
    = Increment
    | Decrement
    | SetCounter (Result Http.Error Int)
    | Echo String


encodeMsg : Msg -> Maybe Value
encodeMsg msg =
    let
        maybe =
            case msg of
                Increment ->
                    Just "Increment"

                Decrement ->
                    Just "Decrement"

                _ ->
                    Nothing
    in
    Maybe.map (\str -> Encode.object [ ( "msg", Encode.string str ) ]) maybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        Increment ->
            ( model, Cmd.none )

        Decrement ->
            ( model, Cmd.none )

        SetCounter result ->
            ( { model | counter = Result.mapError toString result }
            , Cmd.none
            )

        Echo str ->
            ( { model | echo = str }, Cmd.none )
    )
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, sendMsg msg ])


url : String
url =
    "http://localhost:8081/counter"


getCounter : Cmd Msg
getCounter =
    Http.get url counterDecoder
        |> Http.send SetCounter


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    let
        send json =
            Http.post url (Http.jsonBody json) counterDecoder
                |> Http.send SetCounter
    in
    encodeMsg msg
        |> Maybe.map send
        |> Maybe.withDefault Cmd.none



-- View


viewCounter : Int -> Html Msg
viewCounter counter =
    Html.div [] <|
        [ Html.button [ Events.onClick Decrement ] [ Html.text "-" ]
        , Html.text <| toString counter
        , Html.button [ Events.onClick Increment ] [ Html.text "+" ]
        ]


view : Model -> Html Msg
view { counter, echo } =
    Result.map viewCounter counter
        |> Result.mapError Html.text
        |> unwrap
        |> (\c -> Html.main_ [] [ c, Html.text echo ])



-- helpers


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok a ->
            a

        Err a ->
            a
