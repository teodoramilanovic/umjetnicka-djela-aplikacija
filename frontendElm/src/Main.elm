module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S
import Svg.Attributes as SA
import Tuple3

type alias Model =
    { artworkPeriod : String
    , artworkArtist : String
    , artworkMedium : String
    , addTitle : String
    , addFormId : String
    , addArtistId : String
    , addLocation : String
    , addPeriodId : String
    , addDimensions : String
    , addMediumId : String
    , addArtworkPicture : String
    , addArtworkDescription : String
    , addName : String
    , addYearOfBirth : String
    , addYearOfDeath : String
    , addNationality : String
    , addArtistPicture : String
    , addArtistDescription : String
    , getArtist : String
    , getArtwork : String
    , results1 : List (Artwork,Artist,Period)
    , results2 : List Artist
    , errorMessage : Maybe String
    , current : Int
    , currentArtworkForm : Int
    , currentAdd : Bool
    , currentAddItem : Int
    , currentArtwork : Maybe Artwork
    , currentArtist : Maybe Artist
    , currentPeriod : Maybe Period
    , currentMedium : Maybe Medium
    , loading : Bool
    }


type alias Artwork =
    { title : String
    , location : String
    , dimensions : String
    , picture : Maybe String
    , description : Maybe String
    , mediumId : Int
    , id : Int
    }

type alias Artist =
    { name : String
    , yearOfBirth : Int
    , yearOfDeath : Int
    , nationality : String
    , picture : Maybe String
    , description : Maybe String
    , id : Int
    }

type alias Period =
    { name : String
    , id : Int
    }

type alias Medium =
    { name : String
    }

type Msg
    = MsgGetAllArtworks
    | MsgGetAllPaintings
    | MsgGetAllSculptures
    | MsgGetAllArtists
    | MsgInputPeriodField String
    | MsgInputArtistField String
    | MsgInputMediumField String
    | MsgInputTitleField String
    | MsgInputFormIdField String
    | MsgInputArtistIdField String
    | MsgInputLocationField String
    | MsgInputPeriodIdField String
    | MsgInputDimensionsField String
    | MsgInputMediumIdField String
    | MsgInputArtworkPictureField String
    | MsgInputArtworkDescriptionField String
    | MsgInputNameField String
    | MsgInputYearOfBirthField String
    | MsgInputYearOfDeathField String
    | MsgInputNationalityField String
    | MsgInputArtistPictureField String
    | MsgInputArtistDescriptionField String
    | MsgGetArtistField String
    | MsgGetArtworkField String
    | MsgGotResults1 (Result Http.Error (List (Artwork,Artist,Period)))
    | MsgGotResults2 (Result Http.Error (List Artist))
    | MsgSuccesfulPost (Result Http.Error ())
    | MsgSuccessfulDelete (Result Http.Error ())
    | MsgFilter
    | MsgShowAddArtwork
    | MsgShowAddArtist
    | MsgAddArtwork
    | MsgAddArtist
    | MsgGetArtist
    | MsgGetArtwork
    | MsgDeleteArtist Int
    | MsgDeleteArtwork Int
    | MsgShowArtworkInformation (Maybe Artwork) (Maybe Artist) (Maybe Period)
    | MsgShowArtistInformation (Maybe Artist)
    | MsgSortArtwork
    | MsgSortArtist
    | MsgGotMedium (Result Http.Error (Maybe Medium))

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchArtworks )


initModel : Model
initModel =
    { artworkPeriod = ""
    , artworkArtist = ""
    , artworkMedium = ""
    , addTitle = ""
    , addFormId = ""
    , addArtistId = ""
    , addLocation = ""
    , addPeriodId = ""
    , addDimensions = ""
    , addMediumId = ""
    , addArtworkPicture = ""
    , addArtworkDescription = ""
    , addName = ""
    , addYearOfBirth = ""
    , addYearOfDeath = ""
    , addNationality = ""
    , addArtistPicture = ""
    , addArtistDescription = ""
    , getArtist = ""
    , getArtwork = ""
    , results1 = []
    , results2 = []
    , errorMessage = Nothing
    , current = 0
    , currentArtworkForm = 0
    , currentAdd = False
    , currentAddItem = 0
    , currentArtwork = Nothing
    , currentArtist = Nothing
    , currentPeriod = Nothing
    , currentMedium = Nothing
    , loading = False
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html.Html Msg
view model =
    viewLayout model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputPeriodField newPeriod ->
            ( { model | artworkPeriod = newPeriod }, Cmd.none )

        MsgInputArtistField newArtist ->
            ( { model | artworkArtist = newArtist }, Cmd.none )

        MsgInputMediumField newMedium ->
            ( { model | artworkMedium = newMedium }, Cmd.none )
        
        MsgInputTitleField new ->
            ( { model | addTitle = new }, Cmd.none )

        MsgInputFormIdField new ->
            ( { model | addFormId = new }, Cmd.none )

        MsgInputArtistIdField new ->
            ( { model | addArtistId = new }, Cmd.none )
        
        MsgInputLocationField new ->
            ( { model | addLocation = new }, Cmd.none )

        MsgInputPeriodIdField new ->
            ( { model | addPeriodId = new }, Cmd.none )

        MsgInputDimensionsField new ->
            ( { model | addDimensions = new}, Cmd.none )

        MsgInputMediumIdField new ->
            ( { model | addMediumId = new }, Cmd.none )

        MsgInputArtworkPictureField new ->
            ( { model | addArtworkPicture = new}, Cmd.none )

        MsgInputArtworkDescriptionField new ->
            ( { model | addArtworkDescription = new }, Cmd.none )
        
        MsgInputNameField new ->
            ( { model | addName = new}, Cmd.none )

        MsgInputYearOfBirthField new ->
            ( { model | addYearOfBirth = new }, Cmd.none )

        MsgInputYearOfDeathField new ->
            ( { model | addYearOfDeath = new }, Cmd.none )

        MsgInputNationalityField new ->
            ( { model | addNationality = new }, Cmd.none )

        MsgInputArtistPictureField new ->
            ( { model | addArtistPicture = new }, Cmd.none )

        MsgInputArtistDescriptionField new ->
            ( { model | addArtistDescription = new }, Cmd.none )
        
        MsgGetArtistField new ->
            ( { model | getArtist = new }, Cmd.none )

        MsgGetArtworkField new ->
            ( { model | getArtwork = new }, Cmd.none )

        MsgGotResults1 result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results1 = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus err ->
                                    String.fromInt err

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )
        
        MsgGotResults2 result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results2 = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "Bad status"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )
        
        
        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing, currentAdd = False, addTitle = "", addFormId = "", addArtistId = "", addLocation = "", addPeriodId = "", addDimensions = "", addMediumId = "", addArtworkPicture = "", addArtworkDescription = "", addName = "", addYearOfBirth = "", addYearOfDeath = "", addNationality = "", addArtistPicture = "", addArtistDescription = ""}, if(model.currentAddItem==0) then cmdSearchArtworks else cmdSearchArtists )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus err ->
                                    String.fromInt err

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )
        
        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, if(model.current==0) then (if (model.currentArtworkForm==0) then cmdSearchArtworks else if (model.currentArtworkForm==1) then cmdSearchPaintings else cmdSearchSculptures) else cmdSearchArtists)

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus err ->
                                    String.fromInt err

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none)

        MsgGetAllArtworks ->
            updateArtworks model
        
        MsgGetAllPaintings ->
            updatePaintings model

        MsgGetAllSculptures ->
            updateSculptures model

        MsgGetAllArtists ->
            updateArtists model

        MsgFilter ->
            filterResults model

        MsgShowAddArtwork ->
            ( { model | current = 0, currentAdd = True, currentAddItem = 0, results1 = [], results2 = [], currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing }, Cmd.none )

        MsgShowAddArtist ->
            ( { model | current = 1, currentAdd = True, currentAddItem = 1, results1 = [], results2 = [], currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing }, Cmd.none )

        MsgAddArtwork ->
            addArtwork model
        
        MsgAddArtist ->
            addArtist model
        
        MsgGetArtwork ->
            getArtwork model
        
        MsgGetArtist ->
            getArtist model

        MsgDeleteArtwork id ->
            deleteArtwork model id
        
        MsgDeleteArtist id ->
            deleteArtist model id
        
        MsgShowArtworkInformation artwork artist period ->
            ( { model | currentArtwork = artwork, currentArtist = artist, currentPeriod = period, results1 = [], results2 = [] }, 
            cmdGetMediumName (case artwork of 
            Just a -> a.mediumId 
            Nothing -> 0) )
        
        MsgShowArtistInformation artist ->
            ( { model | currentArtist = artist, results1 = [], results2 = [] }, Cmd.none )
        
        MsgSortArtwork ->
            sortArtwork model
        
        MsgSortArtist ->
            sortArtist model
        
        MsgGotMedium result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing, currentMedium=data}, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus err ->
                                    String.fromInt err

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )




updateArtworks : Model -> ( Model, Cmd Msg )
updateArtworks model =
    ( { model | loading = True, current = 0, currentArtworkForm=0, currentAdd = False, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing }, cmdSearchArtworks )

updatePaintings : Model -> ( Model, Cmd Msg )
updatePaintings model =
    ( { model | loading = True, current = 0, currentArtworkForm=1, currentAdd = False, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing}, cmdSearchPaintings )

updateSculptures : Model -> ( Model, Cmd Msg )
updateSculptures model =
    ( { model | loading = True, current = 0, currentArtworkForm=2, currentAdd = False, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing }, cmdSearchSculptures )

updateArtists : Model -> ( Model, Cmd Msg )
updateArtists model =
    ( { model | loading = True, current = 1, currentAdd = False, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing }, cmdSearchArtists )

filterResults : Model -> ( Model, Cmd Msg )
filterResults model =
    ( { model | loading = True, current = 0, currentArtworkForm=0, currentAdd = False }, cmdFilterResults model )

addArtwork : Model -> ( Model, Cmd Msg )
addArtwork model =
    ( { model | loading = True }, cmdPostArtwork model )

addArtist : Model -> ( Model, Cmd Msg )
addArtist model =
    ( { model | loading = True }, cmdPostArtist model )

getArtwork : Model -> ( Model, Cmd Msg )
getArtwork model =
    ( { model | loading = True, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing }, cmdGetArtwork model )

getArtist : Model -> ( Model, Cmd Msg )
getArtist model =
    ( { model | loading = True, currentArtist = Nothing, currentArtwork = Nothing, currentPeriod = Nothing, currentMedium = Nothing }, cmdGetArtist model )

deleteArtwork : Model -> Int -> ( Model, Cmd Msg )
deleteArtwork model id =
    ( { model | loading = True }, cmdDeleteArtwork id )

deleteArtist : Model -> Int -> ( Model, Cmd Msg )
deleteArtist model id =
    ( { model | loading = True }, cmdDeleteArtist id )

sortArtwork : Model -> ( Model, Cmd Msg )
sortArtwork model =
    ( { model | loading = True }, if (model.currentArtworkForm==0) then cmdSortArtwork else if (model.currentArtworkForm==1) then cmdSortPaintings else cmdSortSculptures)

sortArtist : Model -> ( Model, Cmd Msg )
sortArtist model =
    ( { model | loading = True }, cmdSortArtist )



cmdSearchArtworks : Cmd Msg
cmdSearchArtworks =
    Http.get
        { url = "http://localhost:5000/artworks"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdSearchPaintings : Cmd Msg
cmdSearchPaintings =
    Http.get
        { url = "http://localhost:5000/paintings"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdSearchSculptures : Cmd Msg
cmdSearchSculptures =
    Http.get
        { url = "http://localhost:5000/sculptures"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdSearchArtists : Cmd Msg
cmdSearchArtists =
    Http.get
        { url = "http://localhost:5000/artists"
        , expect = Http.expectJson MsgGotResults2 decodeArtists 
        }

cmdFilterResults : Model -> Cmd Msg
cmdFilterResults model =
    Http.get
        { url = "http://localhost:5000/artworks/filter/period=" ++ model.artworkPeriod ++ "/artist=" ++ model.artworkArtist ++ "/medium=" ++ model.artworkMedium
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdPostArtwork : Model -> Cmd Msg
cmdPostArtwork model =
    Http.post
        { url = "http://localhost:5000/artworks"
        , body = jsonBody (encodeArtwork model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }

cmdPostArtist : Model -> Cmd Msg
cmdPostArtist model =
    Http.post
        { url = "http://localhost:5000/artists"
        , body = jsonBody (encodeArtist model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }

cmdGetArtwork : Model -> Cmd Msg
cmdGetArtwork model =
    Http.get
        { url = "http://localhost:5000/artworks/" ++ model.getArtwork
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdGetArtist : Model -> Cmd Msg
cmdGetArtist model =
    Http.get
        { url = "http://localhost:5000/artists/"  ++  model.getArtist
        , expect = Http.expectJson MsgGotResults2 decodeArtists
        }

cmdDeleteArtwork : Int -> Cmd Msg
cmdDeleteArtwork id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5000/artworks/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        , timeout = Nothing
        , tracker = Nothing
        }

cmdDeleteArtist : Int -> Cmd Msg
cmdDeleteArtist id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5000/artists/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        , timeout = Nothing
        , tracker = Nothing
        }

cmdSortArtwork : Cmd Msg
cmdSortArtwork =
    Http.get
        { url = "http://localhost:5000/artworks/sort/1"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdSortArtist : Cmd Msg
cmdSortArtist =
    Http.get
        { url = "http://localhost:5000/artists/sort/1"
        , expect = Http.expectJson MsgGotResults2 decodeArtists
        }

cmdSortPaintings : Cmd Msg
cmdSortPaintings =
    Http.get
        { url = "http://localhost:5000/paintings/sort/1"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdSortSculptures : Cmd Msg
cmdSortSculptures =
    Http.get
        { url = "http://localhost:5000/sculptures/sort/1"
        , expect = Http.expectJson MsgGotResults1 decodeExtendedArtworks
        }

cmdGetMediumName : Int -> Cmd Msg
cmdGetMediumName mid =
    Http.get
        { url = "http://localhost:5000/mediums/"  ++  String.fromInt mid
        , expect = Http.expectJson MsgGotMedium (JD.maybe decodeMedium)
        }

viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layout
        []
        (E.column [ E.paddingXY 5 5, E.spacing 5, E.centerX] 
            [ viewSearchBar model
            , viewErrorMessage model
            , viewGetAndResults model
            , viewInformation model
            , viewAddBar model
            ]
        )

viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column [EBG.color (E.rgb255 209 181 175) ]
            [ E.row [ E.spacing 10, E.paddingXY 10 20, E.centerX ]
            [ viewGetArtworksButton
            , viewGetPaintingsButton
            , viewGetSculpturesButton
            , viewGetArtistsButton
            , viewAddArtworkButton
            , viewAddArtistButton
            ],
            E.row [ E.spacing 20, E.paddingEach {top=0, right=20, bottom=20, left=20}, E.centerX ]
            [ EI.search [E.height (E.px 43), E.width (E.px 230)]
                { onChange = MsgInputPeriodField
                , text = model.artworkPeriod
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Period")
                }
            , EI.search [E.height (E.px 43), E.width (E.px 230)]
                { onChange = MsgInputArtistField
                , text = model.artworkArtist
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Artist")
                }
            , EI.search [E.height (E.px 43), E.width (E.px 230)]
                { onChange = MsgInputMediumField
                , text = model.artworkMedium
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Medium")
                }
            , viewFilterButton
            ]
        ]

viewErrorMessage : Model -> E.Element Msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.paragraph [ EF.bold, EF.color (E.rgb255 145 17 37) ] [ E.text errorMessage ]

        Nothing ->
            E.none

viewGetAndResults : Model -> E.Element Msg
viewGetAndResults model = if (model.currentAdd == False && (model.currentArtwork==Nothing && model.currentArtist==Nothing && model.currentPeriod==Nothing)) then (
        E.column [ E.padding 20, E.spacing 20, EB.color (E.rgb255 156 118 110) ]
            [ viewGetBar model
            , viewResults model
            ] 
        ) else (E.none)

viewResults : Model -> E.Element Msg
viewResults model =
    E.wrappedRow [ E.spacing 20, E.centerX, E.width (E.px 1000) ] 
        (if (model.current==0) then (List.map viewArtwork model.results1) else (List.map viewArtist model.results2))

viewInformation : Model -> E.Element Msg
viewInformation model = 
        if(model.current==0) then (
            viewArtworkInformation model
        )else (
           viewArtistInformation model
        )

viewArtworkInformation : Model -> E.Element Msg
viewArtworkInformation model = 
            case model.currentArtwork of
                Just artwork ->
                    let
                        titleE =
                            E.paragraph [ EF.bold, EF.size 20, E.paddingEach {top=10,right=0,bottom=5,left=0}, EF.color (E.rgb255 54 33 28) ] [ E.text artwork.title ]
                        
                        nameE =
                            case model.currentArtist of
                                Just artist ->
                                    E.paragraph [ EF.size 18, E.paddingXY 0 5, EF.color (E.rgb255 54 33 28) ] [ E.text artist.name ]

                                Nothing ->
                                    E.none

                        pictureE =
                            case artwork.picture of
                                Just picture ->
                                    viewItemPictureBig picture artwork.title

                                Nothing ->
                                    E.none
                        
                        periodE = case model.currentPeriod of
                                Just period ->
                                    E.paragraph [ EF.size 17, EF.center, E.width (E.px 200), E.paddingXY 0 5, EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text period.name ]

                                Nothing ->
                                    E.none
                        
                        mediumE = case model.currentMedium of
                                Just medium ->
                                    E.paragraph [ EF.size 17, EF.center, E.width (E.px 200), E.paddingXY 0 5, EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text medium.name ]

                                Nothing ->
                                    E.none

                        locationE =
                                E.paragraph [ EF.size 17, EF.center, E.width (E.px 200), EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text artwork.location]

                        dimensionsE =
                                E.paragraph [ EF.size 17, EF.center, E.width (E.px 200), EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text artwork.dimensions]

                        descriptionE = case artwork.description of
                                Just description ->
                                    E.paragraph [ EF.size 17, EF.justify, E.padding 10, EF.color (E.rgb255 54 33 28) ] [ E.text description]

                                Nothing ->
                                    E.none
                                

                    in
                    E.column
                    [ E.width (E.px 800)
                    , E.height (E.px 800)
                    , E.padding 15
                    , E.centerX
                    ] [ E.column 
                        [ EBG.color (E.rgb255 156 118 110)
                        , E.width (E.px 400)
                        , E.height (E.px 600)
                        , E.centerX
                        , EB.rounded 20
                        ] [
                        E.row [E.centerX][titleE]
                        , E.row [E.centerX][nameE]
                        , E.row [E.centerX][pictureE]
                        , E.row [E.centerX][periodE, mediumE]
                        , E.row [E.centerX][locationE, dimensionsE]
                        ]
                        , descriptionE
                    ]

                Nothing ->
                    E.none

viewArtistInformation : Model -> E.Element Msg
viewArtistInformation model = 
            case model.currentArtist of
                Just artist ->
                    let
                        nameE =
                             E.paragraph [ EF.bold, EF.size 20, E.paddingEach {top=10,right=0,bottom=5,left=0}, EF.color (E.rgb255 54 33 28) ] [ E.text artist.name ]

                        pictureE =
                            case artist.picture of
                                Just picture ->
                                    viewItemPictureBig picture artist.name

                                Nothing ->
                                    E.none

                        yearsE =
                                E.paragraph [ EF.size 17, E.padding 10, EF.center, E.width (E.px 200), EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text (String.fromInt artist.yearOfBirth ++ " - " ++ String.fromInt artist.yearOfDeath)]

                        nationalityE =
                                E.paragraph [ EF.size 17, EF.center, E.width (E.px 200), EF.color (E.rgb255 0xEE 0xEE 0xEE) ] [ E.text artist.nationality]

                        descriptionE = case artist.description of
                                Just description ->
                                    E.paragraph [ EF.size 17, EF.justify, E.padding 10, EF.color (E.rgb255 54 33 28) ] [ E.text description]

                                Nothing ->
                                    E.none

                    in
                    E.column
                    [ E.width (E.px 800)
                    , E.height (E.px 800)
                    , E.padding 15
                    , E.centerX
                    ] [ E.column 
                        [ EBG.color (E.rgb255 156 118 110)
                        , E.width (E.px 400)
                        , E.height (E.px 570)
                        , E.centerX
                        , EB.rounded 20
                        ] [E.row [E.centerX][nameE]
                        , E.row [E.centerX][pictureE]
                        , E.row [E.centerX][yearsE]
                        , E.row [E.centerX][nationalityE]
                        ]
                        , descriptionE
                    ]

                Nothing ->
                    E.none

viewGetBar : Model -> E.Element Msg
viewGetBar model = if (model.currentAdd==False && model.current==0 ) then viewGetArtworkBar model 
                        else if (model.currentAdd==False && model.current==1) then viewGetArtistBar model
                            else (
                                E.row [][]
                            )

viewAddBar : Model -> E.Element Msg
viewAddBar model = if (model.currentAdd && model.current==0) then viewAddArtworkBar model 
                        else if (model.currentAdd && model.current==1) then viewAddArtistBar model
                            else (
                                E.column [][]
                            )

viewGetArtworkBar : Model -> E.Element Msg
viewGetArtworkBar model = 
        E.row [E.spacing 430]
            [E.row [E.spacing 15] [EI.search [E.height (E.px 43), E.width (E.px 200)]
                { onChange = MsgGetArtworkField
                , text = model.getArtwork
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 156 118 110) ] (E.text "Insert title")
                }
            , viewGetArtworkButton] 
            , sortByTitleButton
            ]

viewGetArtistBar : Model -> E.Element Msg
viewGetArtistBar model = 
        E.row [E.spacing 390, E.paddingXY 5 0]
            [E.row [E.spacing 15] [EI.search [E.height (E.px 43), E.width (E.px 200)]
                { onChange = MsgGetArtistField
                , text = model.getArtist
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 156 118 110) ] (E.text "Insert name")
                }
            , viewGetArtistButton ]
            , sortByNameButton
            ]

viewAddArtworkBar : Model -> E.Element Msg
viewAddArtworkBar model =
        E.column [ E.spacing 10, E.paddingXY 20 20, EBG.color (E.rgb255 209 181 175), E.centerX ]
            [ EI.search []
                { onChange = MsgInputTitleField
                , text = model.addTitle
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Title")
                }
            , EI.search []
                { onChange = MsgInputFormIdField
                , text = model.addFormId
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Form Id")
                }
            , EI.search []
                { onChange = MsgInputArtistIdField
                , text = model.addArtistId
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Artist Id")
                }
            , EI.search []
                { onChange = MsgInputLocationField
                , text = model.addLocation
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Location")
                }
            , EI.search []
                { onChange = MsgInputPeriodIdField
                , text = model.addPeriodId
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Period Id")
                }
            , EI.search []
                { onChange = MsgInputDimensionsField
                , text = model.addDimensions
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Dimensions")
                }
            , EI.search []
                { onChange = MsgInputMediumIdField
                , text = model.addMediumId
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Medium Id")
                }
            , EI.search []
                { onChange = MsgInputArtworkPictureField
                , text = model.addArtworkPicture
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Picture")
                }
            , EI.search []
                { onChange = MsgInputArtworkDescriptionField
                , text = model.addArtworkDescription
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Description")
                }  
            , viewAddArtworkButton2
            ] 

viewAddArtistBar : Model -> E.Element Msg
viewAddArtistBar model =
        E.column [ E.spacing 10, E.paddingXY 20 20, EBG.color (E.rgb255 209 181 175), E.centerX ]
            [ EI.search []
                { onChange = MsgInputNameField
                , text = model.addName
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Name")
                }
            , EI.search []
                { onChange = MsgInputYearOfBirthField
                , text = model.addYearOfBirth
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Year of birth")
                }
            , EI.search []
                { onChange = MsgInputYearOfDeathField
                , text = model.addYearOfDeath
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Year of death")
                }
            , EI.search []
                { onChange = MsgInputNationalityField
                , text = model.addNationality
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Nationality")
                }
            , EI.search []
                { onChange = MsgInputArtistPictureField
                , text = model.addArtistPicture
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Picture")
                }
            , EI.search []
                { onChange = MsgInputArtistDescriptionField
                , text = model.addArtistDescription
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 255 255 255) ] (E.text "Description")
                }
            , viewAddArtistButton2
            ]


viewArtwork : (Artwork,Artist,Period) -> E.Element Msg
viewArtwork (artwork,artist,period) =
    let
        titleE =
            E.paragraph [ EF.bold, E.width (E.px 230), EF.size 20, EF.center, E.paddingXY 5 5, EF.color (E.rgb255 54 33 28) ] [ E.text artwork.title ]
        
        nameE =
            E.paragraph [ EF.size 17, EF.center, E.paddingEach {top=0,right=0,bottom=5,left=0}, EF.color (E.rgb255 54 33 28) ] [ E.text artist.name ]

        pictureE =
            case artwork.picture of
                Just picture ->
                    viewItemPicture picture artwork.title

                Nothing ->
                    E.none

        viewButtonE = 
            EI.button
            [ EBG.color (E.rgb255 156 118 110)
            , EF.color (E.rgb255 0xEE 0xEE 0xEE)
            , EB.rounded 5
            , E.padding 8
            , E.mouseOver
                [ EBG.color (E.rgb255 110 79 72)
                , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                ]
            , E.focused
                [ EBG.color (E.rgb255 110 79 72)
                , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                ]
            , E.height (E.px 40)
            , E.width (E.px 90)
            , EF.center
            ]
            { onPress = Just (MsgShowArtworkInformation (Just artwork) (Just artist) (Just period))
            , label = E.text "View"
            }

        deleteButtonE = 
            EI.button
                [ EBG.color (E.rgb255 156 118 110)
                , EF.color (E.rgb255 0xEE 0xEE 0xEE)
                , EB.rounded 5
                , E.padding 8
                , E.mouseOver
                    [ EBG.color (E.rgb255 110 79 72)
                    , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                    ]
                , E.focused
                    [ EBG.color (E.rgb255 110 79 72)
                    , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                    ]
                , E.height (E.px 40)
                , E.width (E.px 90)
                , EF.center
                ]
                { onPress = Just (MsgDeleteArtwork artwork.id)
                , label = E.text "Delete"
                }

    in
    E.column
        [ E.width (E.px 230)
        , E.height (E.px 380)
        , EBG.color (E.rgb255 209 181 175)
        , E.padding 15
        ] [ E.row [E.centerX][pictureE]
            , E.row [E.centerX][titleE]
            , E.row [E.centerX][nameE]
            , E.row [ E.centerX, E.spacing 10 ]
                [ viewButtonE
                , deleteButtonE
                ]
            ]

viewArtist : Artist -> E.Element Msg
viewArtist artist =
    let
        nameE =
            E.paragraph [ EF.bold, E.width (E.px 230), EF.size 20, EF.center, E.paddingXY 5 5, EF.color (E.rgb255 54 33 28) ] [ E.text artist.name ]

        pictureE =
            case artist.picture of
                Just picture ->
                    viewItemPicture picture artist.name

                Nothing ->
                    E.none
        
        viewButtonE = EI.button
            [ EBG.color (E.rgb255 156 118 110)
            , EF.color (E.rgb255 0xEE 0xEE 0xEE)
            , EB.rounded 5
            , E.padding 8
            , E.mouseOver
                [ EBG.color (E.rgb255 110 79 72)
                , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                ]
            , E.focused
                [ EBG.color (E.rgb255 110 79 72)
                , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                ]
            , E.height (E.px 40)
            , E.width (E.px 90)
            , EF.center
            ]
            { onPress = Just (MsgShowArtistInformation (Just artist))
            , label = E.text "View"
            }

        deleteButtonE = EI.button
            [ EBG.color (E.rgb255 156 118 110)
                , EF.color (E.rgb255 0xEE 0xEE 0xEE)
                , EB.rounded 5
                , E.padding 8
                , E.mouseOver
                    [ EBG.color (E.rgb255 110 79 72)
                    , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                    ]
                , E.focused
                    [ EBG.color (E.rgb255 110 79 72)
                    , EF.color (E.rgb255 0xDD 0xDD 0xDD)
                    ]
                , E.height (E.px 40)
                , E.width (E.px 90)
                , EF.center
                ]
            { onPress = Just (MsgDeleteArtist artist.id)
            , label = E.text "Delete"
            }

    in
    E.column
        [ E.width (E.px 230)
        , E.height (E.px 350)
        , EBG.color (E.rgb255 209 181 175)
        , E.padding 15
        ] [ E.row [E.centerX][pictureE]
            , E.row [E.centerX][nameE]
            , E.row [ E.centerX, E.spacing 10 ]
                [ viewButtonE
                , deleteButtonE
                ]
            ]


viewItemPicture : String -> String -> E.Element msg
viewItemPicture picture title =
    E.image [E.width (E.px 200)
        , E.height (E.px 250)
        , EB.rounded 20]
        { src = picture
        , description = title
        }

viewItemPictureBig : String -> String -> E.Element msg
viewItemPictureBig picture title =
    E.image [E.width (E.px 300)
        , E.height (E.px 450)
        , EB.rounded 20]
        { src = picture
        , description = title
        }

viewGetArtworksButton : E.Element Msg
viewGetArtworksButton =
    viewButtonGeneric "All Artworks" MsgGetAllArtworks

viewGetPaintingsButton : E.Element Msg
viewGetPaintingsButton =
    viewButtonGeneric "All Paintings" MsgGetAllPaintings

viewGetSculpturesButton : E.Element Msg
viewGetSculpturesButton =
    viewButtonGeneric "All Sculptures" MsgGetAllSculptures

viewGetArtistsButton : E.Element Msg
viewGetArtistsButton =
    viewButtonGeneric "All Artists" MsgGetAllArtists

viewAddArtworkButton : E.Element Msg
viewAddArtworkButton =
    viewButtonGeneric "Add Artwork" MsgShowAddArtwork

viewAddArtistButton : E.Element Msg
viewAddArtistButton =
    viewButtonGeneric "Add Artist" MsgShowAddArtist

viewFilterButton : E.Element Msg
viewFilterButton =
    viewButtonGeneric "Filter" MsgFilter

viewAddArtworkButton2 : E.Element Msg
viewAddArtworkButton2 =
    viewButtonGeneric "Add Artwork" MsgAddArtwork

viewAddArtistButton2 : E.Element Msg
viewAddArtistButton2 =
    viewButtonGeneric "Add Artist" MsgAddArtist

viewGetArtworkButton : E.Element Msg
viewGetArtworkButton =
    viewButtonGeneric "Search" MsgGetArtwork

viewGetArtistButton : E.Element Msg
viewGetArtistButton =
    viewButtonGeneric "Search" MsgGetArtist

sortByTitleButton : E.Element Msg
sortByTitleButton =
    viewButtonGeneric "Sort By Title" MsgSortArtwork

sortByNameButton : E.Element Msg
sortByNameButton =
    viewButtonGeneric "Sort By Name" MsgSortArtist

viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric title msg =
    EI.button
        [ EBG.color (E.rgb255 156 118 110)
        , EF.color (E.rgb255 0xEE 0xEE 0xEE)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EBG.color (E.rgb255 110 79 72)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        , E.focused
            [ EBG.color (E.rgb255 110 79 72)
            , EF.color (E.rgb255 0xDD 0xDD 0xDD)
            ]
        ]
        { onPress = Just msg
        , label = E.text title
        }



encodeArtwork : Model -> JE.Value
encodeArtwork model =
    JE.object
        [ ( "title", JE.string model.addTitle )
        , ( "formId", JE.int (Maybe.withDefault 0 (String.toInt model.addFormId)) )
        , ( "artistId", JE.int (Maybe.withDefault 0 (String.toInt model.addArtistId)) )
        , ( "location", JE.string model.addLocation )
        , ( "periodId", JE.int (Maybe.withDefault 0 (String.toInt model.addPeriodId)) )
        , ( "dimensions", JE.string model.addDimensions )
        , ( "mediumId", JE.int (Maybe.withDefault 0 (String.toInt model.addMediumId)) )
        , ( "picture", JE.string model.addArtworkPicture )
        , ( "description", JE.string model.addArtworkDescription )
        ]

encodeArtist : Model -> JE.Value
encodeArtist model =
    JE.object
        [ ( "name", JE.string model.addName )
        , ( "yearOfBirth", JE.int (Maybe.withDefault 0 (String.toInt model.addYearOfBirth)) )
        , ( "yearOfDeath", JE.int (Maybe.withDefault 0 (String.toInt model.addYearOfDeath)) )
        , ( "nationality", JE.string model.addNationality )
        , ( "picture", JE.string model.addArtistPicture )
        , ( "description", JE.string model.addArtistDescription )
        ]


andMap = JD.map2 (|>)

decodeArtworks : JD.Decoder (List Artwork)
decodeArtworks =
    JD.list decodeArtwork

decodeArtwork : JD.Decoder Artwork
decodeArtwork =
        JD.succeed Artwork
        |> andMap (JD.field "title" JD.string)
        |> andMap (JD.field "location" JD.string)
        |> andMap (JD.field "dimensions" JD.string)
        |> andMap (JD.maybe (JD.field "picture" JD.string))
        |> andMap (JD.maybe (JD.field "description" JD.string))
        |> andMap (JD.field "mediumId" JD.int)
        |> andMap (JD.field "id" JD.int)

decodeArtists : JD.Decoder (List Artist)
decodeArtists =
    JD.list decodeArtist

decodeArtist : JD.Decoder Artist
decodeArtist =
        JD.succeed Artist
        |> andMap (JD.field "name" JD.string)
        |> andMap (JD.field "yearOfBirth" JD.int)
        |> andMap (JD.field "yearOfDeath" JD.int)
        |> andMap (JD.field "nationality" JD.string)
        |> andMap (JD.maybe (JD.field "picture" JD.string))
        |> andMap (JD.maybe (JD.field "description" JD.string))
        |> andMap (JD.field "id" JD.int)

decodePeriod : JD.Decoder Period
decodePeriod =
        JD.map2 Period
        (JD.field "name" JD.string)
        (JD.field "id" JD.int)

decodeMedium : JD.Decoder Medium
decodeMedium =
        JD.map Medium
        (JD.field "name" JD.string)


decodeExtendedArtworks : JD.Decoder (List (Artwork, Artist, Period))
decodeExtendedArtworks =
    JD.list decodeExtendedArtwork

decodeExtendedArtwork : JD.Decoder (Artwork,Artist,Period)
decodeExtendedArtwork =
        JD.map3 Tuple3.join
        (JD.index 0 decodeArtwork)
        (JD.index 1 decodeArtist)
        (JD.index 2 decodePeriod)