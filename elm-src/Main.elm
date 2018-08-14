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


type Event
    = Increment
    | Decrement


type Msg
    = Increment1
    | Decrement1
    | SetCounter1 (Result Http.Error Int)
    | Increment2
    | Decrement2
    | WSSetCounter String


encodeEvent : Event -> Value
encodeEvent event =
    let
        e =
            case event of
                Increment ->
                    "Increment"

                Decrement ->
                    "Decrement"
    in
    Encode.object [ ( "msg", Encode.string e ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment1 ->
            ( model
            , sendEvent Increment
            )

        Decrement1 ->
            ( model
            , sendEvent Decrement
            )

        SetCounter1 result ->
            ( { model | counter1 = Result.mapError toString result }
            , Cmd.none
            )

        Increment2 ->
            ( model
            , encodeEvent Increment
                |> Encode.encode 0
                |> WebSocket.send streamUri
            )

        Decrement2 ->
            ( model
            , encodeEvent Decrement
                |> Encode.encode 0
                |> WebSocket.send streamUri
            )

        WSSetCounter str ->
            ( { model | counter2 = Decode.decodeString counterDecoder str }
            , Cmd.none
            )


url : String
url =
    "http://localhost:8081/counter"


getCounter : Cmd Msg
getCounter =
    Http.get url counterDecoder
        |> Http.send SetCounter1


sendEvent : Event -> Cmd Msg
sendEvent event =
    Http.post url (Http.jsonBody <| encodeEvent event) counterDecoder
        |> Http.send SetCounter1



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
