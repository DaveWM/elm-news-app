module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, p, input, h5, a)
import Html.Attributes exposing (src)
import Time exposing (utc)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Http
import Json.Decode exposing (Decoder, field, string, list, nullable)
import List
import Debug

---- MODEL ----


type alias Model =
    { time: Time.Posix,
      searchQuery: Maybe String,
      articles: List Article}


init : ( Model, Cmd Msg )
init =
    ({time = (Time.millisToPosix 0), searchQuery = Nothing, articles = []}, Cmd.none )



---- UPDATE ----


type Msg = 
  SearchQueryUpdated String
  | ResponseReceived (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    SearchQueryUpdated query -> 
        let newQuery = if String.isEmpty query then Nothing else Just query in
        (
            {model | searchQuery = newQuery},
            newQuery 
              |> Maybe.map (\q -> Http.get 
                { url = "https://newsapi.org/v2/everything?q=" ++ query ++ "&apiKey=a35ce68466704851bec15046387412f6"
                  , expect = Http.expectJson ResponseReceived responseDecoder} )
              |> Maybe.withDefault Cmd.none
        )
    ResponseReceived (Ok response) -> ({model | articles = response.articles}, Cmd.none)
    ResponseReceived (Err err) -> 
        let _ = Debug.log "Error: " err in
        (model, Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container"]
        [ h1 [] [ text "Elm News Feed" ]
        , input [placeholder "Enter your search query here...", onInput SearchQueryUpdated, class "form-control"] [ ]
        , div []  
            (List.map 
                (\article -> div [class "media article"]
                        [ img [src (Maybe.withDefault "https://via.placeholder.com/80" article.urlToImage), class "mr-3 align-self-center"] []
                        , div [class "media-body"] 
                            [h5 [class "mt-0"] [text article.title]
                            , p [] [text article.description]
                            , a [href article.url] [text "Read More..."]]]) 
                model.articles)
        ]



---- PROGRAM ----


formatDate : Time.Posix -> String
formatDate time = 
   let
    hour   = String.fromInt (Time.toHour utc time)
    minute = String.fromInt (Time.toMinute utc time)
    second = String.fromInt (Time.toSecond utc time)
  in
  hour ++ ":" ++ minute ++ ":" ++ second

type alias Response = 
    {
        articles: List Article
    }

type alias Article = {
    title: String
    , description: String
    , urlToImage: Maybe String
    , url: String
    }

articlesDecoder : Decoder Article  
articlesDecoder = 
  Json.Decode.map4 Article
    (field "title" string)
    (field "description" string)
    (field "urlToImage" (nullable string))
    (field "url" string) 

responseDecoder : Decoder Response
responseDecoder = 
  Json.Decode.map Response
    (field "articles" (list articlesDecoder))

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none }