
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String



main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  {
    content : String,
    content2 : String
  }


model : Model
model = Model "initial text" "initial second text"



-- UPDATE


type Msg
  = Change String | Change2 String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }
    Change2 newContent ->
      { model | content2 = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
  div []
    [ input [ placeholder "Text to reverse", onInput Change, value model.content ] []
    , div [] [ text (String.reverse model.content) ]
    ],
  div []
    [ input [ placeholder "Another text", onInput Change2, value model.content2 ] []
    , div [] [ text model.content2 ]
    ]
  ]
