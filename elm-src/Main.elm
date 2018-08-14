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
    WebSocket.listen "ws://localhost:8081/stream" WSSetCounter



-- MODEL


type alias Model =
    { counter1 : Result String Int
    , counter2 : Result String Int
    }


counterDecoder : Decoder Int
counterDecoder =
    Decode.field "state" Decode.int


init : ( Model, Cmd Msg )
init =
    ( { counter1 = Err "Loading..."
      , counter2 = Err "Connecting to stream"
      }
    , getCounter
    )



-- Update


type Msg
    = Increment1
    | Decrement1
    | SetCounter1 (Result Http.Error Int)
    | WSSetCounter String


encodeMsg : Msg -> Maybe Value
encodeMsg msg =
    let
        maybe =
            case msg of
                Increment1 ->
                    Just "Increment"

                Decrement1 ->
                    Just "Decrement"

                _ ->
                    Nothing
    in
    Maybe.map (\str -> Encode.object [ ( "msg", Encode.string str ) ]) maybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        Increment1 ->
            ( model, Cmd.none )

        Decrement1 ->
            ( model, Cmd.none )

        SetCounter1 result ->
            ( { model | counter1 = Result.mapError toString result }
            , Cmd.none
            )

        WSSetCounter str ->
            ( { model | counter2 = Decode.decodeString counterDecoder str }
            , Cmd.none
            )
    )
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, sendMsg msg ])


url : String
url =
    "http://localhost:8081/counter"


getCounter : Cmd Msg
getCounter =
    Http.get url counterDecoder
        |> Http.send SetCounter1


sendMsg : Msg -> Cmd Msg
sendMsg msg =
    let
        send json =
            Http.post url (Http.jsonBody json) counterDecoder
                |> Http.send SetCounter1
    in
    encodeMsg msg
        |> Maybe.map send
        |> Maybe.withDefault Cmd.none



-- View


viewCounter : ( Msg, Msg ) -> Int -> Html Msg
viewCounter ( increment, decrement ) counter =
    Html.div [] <|
        [ Html.button [ Events.onClick decrement ] [ Html.text "-" ]
        , Html.text <| toString counter
        , Html.button [ Events.onClick increment ] [ Html.text "+" ]
        ]


viewHttpCounter : Result String Int -> Html Msg
viewHttpCounter counter =
    Result.map (viewCounter ( Increment1, Decrement1 )) counter
        |> Result.mapError Html.text
        |> unwrap
        |> List.singleton
        |> Html.div []


view : Model -> Html Msg
view { counter1 } =
    viewHttpCounter counter1



-- helpers


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok a ->
            a

        Err a ->
            a
