module Main exposing (main)

import Html exposing (Html)
import Html.Events as Events
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import WebSocket


-- Settings


streamUri : String
streamUri =
    "ws://localhost:8081/counter/stream"


url : String
url =
    "http://localhost:8081/counter"



-- Main


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
    WebSocket.listen streamUri WSSetCounter



-- Model


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



-- Events


type Event
    = Increment
    | Decrement


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


getCounter : Cmd Msg
getCounter =
    Http.get url counterDecoder
        |> Http.send SetCounter1


sendEvent : Event -> Cmd Msg
sendEvent event =
    Http.post url (Http.jsonBody <| encodeEvent event) counterDecoder
        |> Http.send SetCounter1


emitEvent : Event -> Cmd Msg
emitEvent event =
    encodeEvent event
        |> Encode.encode 0
        |> WebSocket.send streamUri


getEvents : Cmd Msg
getEvents =
    Http.getString (url ++ "/events")
        |> Http.send SetEvents



-- Update


type Msg
    = Increment1
    | Decrement1
    | SetCounter1 (Result Http.Error Int)
    | Increment2
    | Decrement2
    | WSSetCounter String
    | SetEvents (Result Http.Error String)


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
            , Cmd.batch [ emitEvent Increment, getEvents ]
            )

        Decrement2 ->
            ( model
            , emitEvent Decrement
            )

        WSSetCounter str ->
            ( { model | counter2 = Decode.decodeString counterDecoder str }
            , Cmd.none
            )

        SetEvents str ->
            let
                _ =
                    Debug.log "events" str
            in
            ( model, Cmd.none )



-- View


viewCounter : ( Msg, Msg ) -> Int -> Html Msg
viewCounter ( increment, decrement ) counter =
    Html.div [] <|
        [ Html.button [ Events.onClick decrement ] [ Html.text "-" ]
        , Html.text <| toString counter
        , Html.button [ Events.onClick increment ] [ Html.text "+" ]
        ]


viewHttpCounter : ( Msg, Msg ) -> Result String Int -> Html Msg
viewHttpCounter msgs counter =
    Result.map (viewCounter msgs) counter
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



-- Helpers


unwrap : Result a a -> a
unwrap result =
    case result of
        Ok a ->
            a

        Err a ->
            a
