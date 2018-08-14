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


streamUri : String
streamUri =
    "ws://localhost:8081/stream"


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.listen streamUri WSSetCounter



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
    | Increment2
    | Decrement2
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

        Increment2 ->
            ( model
            , encodeMsg Increment1
                |> Maybe.map (WebSocket.send streamUri << Encode.encode 0)
                |> Maybe.withDefault Cmd.none
            )

        Decrement2 ->
            ( model
            , encodeMsg Decrement1
                |> Maybe.map (WebSocket.send streamUri << Encode.encode 0)
                |> Maybe.withDefault Cmd.none
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


viewHttpCounter : ( Msg, Msg ) -> Result String Int -> Html Msg
viewHttpCounter ( increment, decrement ) counter =
    Result.map (viewCounter ( increment, decrement )) counter
        |> Result.mapError Html.text
        |> unwrap
        |> List.singleton
        |> Html.div []


view : Model -> Html Msg
view { counter1, counter2 } =
    Html.main_ []
        [ Html.p [] [ Html.text "This counter uses JSON api to send events to the server." ]
        , viewHttpCounter ( Increment1, Decrement1 ) counter1
        , Html.p [] [ Html.text "This counter uses WebSockets to observe changes on server and push updates back." ]
        , viewHttpCounter ( Increment2, Decrement2 ) counter2
        ]



-- helpers


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok a ->
            a

        Err a ->
            a
