import Html exposing (..)
-- import Regex
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main : Program Never Model Msg
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL

type AccumulatingResult error value
  = Success value
  | Failure (List error)


-- createPassword : String -> Result Password
-- createPassword string = error ""

type alias Validation =
  { message : List String
  }

-- parseAge : Age -> Result Int
-- parseAge ageString = String.toInt ageString.value

-- printAgeError: Result Age -> String
-- printAgeError result
--   = case result of
--   Ok value -> ""
--   Err value -> "Invalid age"

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , validation: Validation
  }


model : Model
model =
  Model "" "" "" "" { message = [] }



-- UPDATE

checkEqual : Model -> Model
checkEqual model = if model.password /= model.passwordAgain
  then
    { model | validation = { message = ["Passwords are not equal", "somethings not right"]}}
  else
    { model | validation = { message = [""] } }

validatePassword : String -> Model
validatePassword password = { model | passwordAgain = password }
  |> checkEqual

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | SetAge String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      validatePassword password

    SetAge age ->
      { model | age = age }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] [ span [] [ text "Name" ], input [ type_ "text", placeholder "Name", onInput Name ] [] ]
    , div [] [ span [] [ text "Password" ], input [ type_ "password", placeholder "Password", onInput Password ] [] ]
    , div [] [ span [] [ text "Re enter password" ], input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] [] ]
    , div [] [ span [] [ text "Age" ], input [ type_ "input", placeholder "Age", onInput SetAge ] [] ]
    , viewValidation model.validation
    ]

getMessage : Maybe String -> String
getMessage msg =
  case msg of
    Just value -> value
    Nothing -> ""

generateValidationMessage: String -> Html msg
generateValidationMessage message =
  div [ ] [ text message ]

generateValidationMessageList: List String -> Html msg
generateValidationMessageList msgList =
  div [ class "validation" ] ( List.map generateValidationMessage msgList )

viewValidation : Validation -> Html msg
viewValidation validation =
  let
    (color, message) = ("green",
    validation.message)
  in
    div [ style [("color", color)] ] [ generateValidationMessageList message ]