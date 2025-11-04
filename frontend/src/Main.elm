module Main exposing (main)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import List.Extra
import String
import Task
import Url exposing (Url)
import Url.Builder exposing (string)
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type ServiceType
    = Apple
    | Youtube
    | Amazon
    | Spotify


type alias Service =
    { name : String
    , icon : String
    , loginLabel : String
    }


type alias Model =
    { key : Browser.Navigation.Key
    , body : Body
    , loginStatuses : Dict String Bool
    , from : Service
    , to : Service
    , currentFromType : ServiceType
    , currentToType : ServiceType
    , fromOptions : List ServiceType
    , currentFromIndex : Int
    , spotifyRaw : Maybe String
    }


type Body
    = Home
    | List
    | Done


type Msg
    = GotLoginStatus (Result Http.Error (Dict String Bool))
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | GoHome
    | GoList
    | GoDone
    | Swap
    | NextService
    | PrevService
    | SendLogin ServiceType
    | LogoutAll
    | FetchSpotifyPlaylists
    | GotSpotifyPlaylists (Result Http.Error String)


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( f, t ) =
            parseFromTo url
    in
    ( { key = key
      , body = Home
      , loginStatuses = Dict.empty
      , from = serviceFromType f
      , to = serviceFromType t
      , currentFromType = f
      , currentToType = t
      , fromOptions = [ Youtube, Apple, Amazon ]
      , currentFromIndex = 1
      , spotifyRaw = Nothing
      }
    , fetchLoginStatus
    )


keyToServiceType : String -> Maybe ServiceType
keyToServiceType k =
    case k of
        "apple" ->
            Just Apple

        "spotify" ->
            Just Spotify

        "youtube" ->
            Just Youtube

        "amazon" ->
            Just Amazon

        _ ->
            Nothing


loginStatusDecoder : D.Decoder (Dict String Bool)
loginStatusDecoder =
    D.dict D.bool


fetchLoginStatus : Cmd Msg
fetchLoginStatus =
    Http.get
        { url = "/api/login/status"
        , expect = Http.expectJson GotLoginStatus loginStatusDecoder
        }


serviceName : ServiceType -> String
serviceName s =
    case s of
        Apple ->
            "AppleMusic"

        Spotify ->
            "Spotify"

        Youtube ->
            "YouTubeMusic"

        Amazon ->
            "AmazonMusic"


serviceKey : ServiceType -> String
serviceKey s =
    case s of
        Apple ->
            "apple"

        Spotify ->
            "spotify"

        Youtube ->
            "youtube"

        Amazon ->
            "amazon"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLoginStatus (Ok dict) ->
            ( { model | loginStatuses = dict }, Cmd.none )

        GotLoginStatus (Err _) ->
            ( model, Cmd.none )

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Task.perform (\_ -> UrlChanged url) (Task.succeed ()) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            let
                ( f, t ) =
                    parseFromTo url

                newModel =
                    { model
                        | currentFromType = f
                        , currentToType = t
                        , from = serviceFromType f
                        , to = serviceFromType t
                    }
            in
            ( newModel, fetchLoginStatus )

        FetchSpotifyPlaylists ->
            ( model
            , Http.get
                { url = "/api/spotify/playlists/raw"
                , expect = Http.expectString GotSpotifyPlaylists
                }
            )

        GotSpotifyPlaylists (Ok raw) ->
            ( { model | spotifyRaw = Just raw }, Cmd.none )

        GotSpotifyPlaylists (Err _) ->
            ( { model | spotifyRaw = Just "{\"error\":\"failsed to fetch\"}" }, Cmd.none )

        NextService ->
            let
                nextIndex =
                    model.currentFromIndex + 1

                lastIndex =
                    List.length model.fromOptions - 1
            in
            if nextIndex > lastIndex then
                ( model, Cmd.none )

            else
                let
                    newType =
                        List.Extra.getAt nextIndex model.fromOptions
                            |> Maybe.withDefault model.currentFromType

                    newService =
                        serviceFromType newType
                in
                ( { model
                    | currentFromIndex = nextIndex
                    , currentFromType = newType
                    , from = newService
                  }
                , Cmd.none
                )

        PrevService ->
            let
                prevIndex =
                    model.currentFromIndex - 1
            in
            if prevIndex < 0 then
                ( model, Cmd.none )

            else
                let
                    newType =
                        List.Extra.getAt prevIndex model.fromOptions
                            |> Maybe.withDefault model.currentFromType

                    newService =
                        serviceFromType newType
                in
                ( { model
                    | currentFromIndex = prevIndex
                    , currentFromType = newType
                    , from = newService
                  }
                , Cmd.none
                )

        Swap ->
            let
                swappedList =
                    List.Extra.setAt model.currentFromIndex model.currentToType model.fromOptions

                newModel =
                    { model
                        | from = model.to
                        , to = model.from
                        , fromOptions = swappedList
                        , currentFromType = model.currentToType
                        , currentToType = model.currentFromType
                    }

                newUrl =
                    "/?from="
                        ++ serviceKey newModel.currentFromType
                        ++ "&to="
                        ++ serviceKey newModel.currentToType
            in
            ( newModel, Browser.Navigation.pushUrl model.key newUrl )

        SendLogin serviceType ->
            let
                stateStr =
                    "from=" ++ serviceKey model.currentFromType ++ "&to=" ++ serviceKey model.currentToType

                encodedState =
                    Url.percentEncode stateStr

                url =
                    case serviceType of
                        Apple ->
                            "https://appleid.apple.com/auth/authorize"
                                ++ "?client_id=com.hasumi.replaylist.login"
                                ++ "&redirect_uri=https://replaylist.ngrok.io/api/login/apple/callback"
                                ++ "&response_type=code"
                                --※ Apple 側は response_mode=query にしておくとサーバでフォーム解析が不要になって楽。
                                ++ "&response_mode=form_post"
                                ++ "&scope=name+email"
                                ++ "&state="
                                ++ stateStr

                        Spotify ->
                            "https://accounts.spotify.com/authorize"
                                ++ "?client_id=a0e8851f25054913bffdfec463b47679"
                                ++ "&response_type=code"
                                ++ "&redirect_uri=https://replaylist.ngrok.io/api/login/spotify/callback"
                                ++ "&scope=playlist-read-private+playlist-modify-private"
                                ++ "&state="
                                ++ encodedState

                        _ ->
                            ""
            in
            ( model, Browser.Navigation.load url )

        GoHome ->
            ( { model | body = Home }, Cmd.none )

        GoList ->
            let
                cmd =
                    if model.currentFromType == Spotify then
                        Task.perform (\_ -> FetchSpotifyPlaylists) (Task.succeed ())

                    else
                        Cmd.none
            in
            ( { model | body = List }, cmd )

        GoDone ->
            ( { model | body = List }, Cmd.none )

        LogoutAll ->
            ( { model | loginStatuses = Dict.empty }
            , Http.post
                { url = "/api/logout_all"
                , body = Http.emptyBody
                , expect = Http.expectWhatever (\_ -> GotLoginStatus (Ok Dict.empty))
                }
            )


serviceFromType : ServiceType -> Service
serviceFromType sType =
    case sType of
        Apple ->
            { name = "AppleMusic"
            , icon = "assets/AppleMusicIcon.png"
            , loginLabel = "Login with AppleID"
            }

        Youtube ->
            { name = "YouTubeMusic"
            , icon = "assets/YouTubeIcon.png"
            , loginLabel = "Login with YouTube"
            }

        Amazon ->
            { name = "AmazonMusic"
            , icon = "assets/AmazonIcon.png"
            , loginLabel = "Login with AmazonMusic"
            }

        Spotify ->
            { name = "Spotify"
            , icon = "assets/SpotifyIcon.png"
            , loginLabel = "Login with Spotify"
            }


parseFromTo : Url -> ( ServiceType, ServiceType )
parseFromTo url =
    let
        fromType k =
            case k of
                Just "apple" ->
                    Apple

                Just "spotify" ->
                    Spotify

                Just "youtube" ->
                    Youtube

                Just "amazon" ->
                    Amazon

                _ ->
                    Apple

        toType k =
            case k of
                Just "apple" ->
                    Apple

                Just "spotify" ->
                    Spotify

                Just "youtube" ->
                    Youtube

                Just "amazon" ->
                    Amazon

                _ ->
                    Spotify
    in
    let
        fromK =
            Query.string "from" |> (\q -> Parser.parse (Parser.top <?> q) url)

        toK =
            Query.string "to" |> (\q -> Parser.parse (Parser.top <?> q) url)
    in
    ( fromK |> Maybe.withDefault Nothing |> fromType
    , toK |> Maybe.withDefault Nothing |> toType
    )


view : Model -> Browser.Document Msg
view model =
    { title = "RE:PLAYLIST"
    , body =
        [ div [ class "app" ]
            [ div [ class "frame" ]
                [ header model
                , bodyView model
                , footerView model
                ]
            ]
        ]
    }


header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ div [ class "header-left" ] [ text "RE:PLAYLIST" ]
        , div [ class "header-right" ]
            [ navLink "home" GoHome (model.body == Home)
            , text " | "
            , navLink "list" GoList (model.body == List)
            , text " | "
            , navLink "done" GoDone (model.body == Done)
            ]
        ]


navLink : String -> Msg -> Bool -> Html Msg
navLink label msg active =
    if active then
        button [ class "nav active" ] [ text label ]

    else
        button [ class "nav", onClick msg ] [ text label ]


bodyView : Model -> Html Msg
bodyView model =
    case model.body of
        Home ->
            div [ class "body" ]
                [ div [ class "card-container" ]
                    [ leftCard model.from model.currentFromType model.currentFromIndex model.fromOptions
                    , div [ class "swap-container" ]
                        [ button [ class "swap-btn", onClick Swap ] [ text "⇄" ] ]
                    , rightCard model.to model.currentToType
                    ]
                ]

        List ->
            case model.currentFromType of
                Spotify ->
                    let
                        shown =
                            model.spotifyRaw |> Maybe.withDefault "loading..."
                    in
                    div [] [ Html.pre [] [ text shown ] ]

                _ ->
                    div [] [ text "List (non-Spotify) is WIP" ]

        Done ->
            div [] [ text "Hello, Body3", button [ onClick GoHome ] [ text "Back to Body1" ] ]


leftCard : Service -> ServiceType -> Int -> List ServiceType -> Html Msg
leftCard service currentFromType currentFromIndex fromOptions =
    let
        disablePrev =
            currentFromIndex == 0

        disableNext =
            currentFromIndex == (List.length fromOptions - 1)
    in
    div [ class "card card-left" ]
        [ div [ class "card-title" ] [ text "FROM:" ]
        , div [ class "card-icon" ]
            [ img [ src service.icon, class "music-icon" ] [] ]
        , div [ class "service-name" ]
            [ button
                [ class "arrow-btn left"
                , onClick PrevService
                , Html.Attributes.disabled disablePrev
                ]
                [ text "◁" ]
            , span [ class "service-label" ] [ text service.name ]
            , button
                [ class "arrow-btn right"
                , onClick NextService
                , Html.Attributes.disabled disableNext
                ]
                [ text "▷" ]
            ]
        , button [ class "login-btn", onClick (SendLogin currentFromType) ] [ text service.loginLabel ]
        ]


rightCard : Service -> ServiceType -> Html Msg
rightCard service currentToType =
    div [ class "card card-right" ]
        [ div [ class "card-title" ] [ text "TO:" ]
        , div [ class "card-icon" ]
            [ img [ src service.icon, class "music-icon" ] [] ]
        , div [ class "service-name-right" ] [ text service.name ]
        , button [ class "login-btn", onClick (SendLogin currentToType) ] [ text service.loginLabel ]
        ]


footerView : Model -> Html Msg
footerView model =
    let
        loggedInList : List ServiceType
        loggedInList =
            model.loginStatuses
                |> Dict.toList
                |> List.filterMap
                    (\( k, v ) ->
                        if v then
                            keyToServiceType k

                        else
                            Nothing
                    )

        bothLoggedIn =
            List.member model.currentFromType loggedInList
                && List.member model.currentToType loggedInList

        loggedInText =
            if List.isEmpty loggedInList then
                "まだログインしていません"

            else
                "Logged in: "
                    ++ (loggedInList
                            |> List.map serviceName
                            |> String.join ", "
                       )
    in
    div [ class "footer" ]
        ([ text loggedInText
         , button [ onClick LogoutAll, class "logout-btn" ] [ text "Logout All" ]
         ]
            ++ (if bothLoggedIn then
                    [ button [ onClick GoList, class "next-btn" ] [ text "Next ➜" ] ]

                else
                    []
               )
        )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
