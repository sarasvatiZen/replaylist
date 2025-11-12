port module Main exposing (main)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (checked, class, src, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Process
import String
import Task
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


port appleLogin : () -> Cmd msg


port receiveAppleUserToken : (String -> msg) -> Sub msg


type alias PlaylistItem =
    { id : String
    , name : String
    , cover : String
    , trackCount : Int
    , checked : Bool
    , tracks : List Track
    }


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


type alias Track =
    { title : String
    , artist : String
    , isrc : Maybe String
    }


type alias Model =
    { key : Browser.Navigation.Key
    , body : Body
    , loginStatuses : Dict String Bool
    , leftList : List ServiceType
    , rightList : List ServiceType
    , leftIndex : Int
    , currentFromType : ServiceType
    , currentToType : ServiceType
    , from : Service
    , to : Service
    , spotifyRaw : Maybe String
    , appleRaw : Maybe String
    , appleUserToken : Maybe String
    , applePlaylists : List PlaylistItem
    , spotifyPlaylists : List PlaylistItem
    , youtubePlaylists : List PlaylistItem
    , isLoading : Bool
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
    | FetchApplePlaylists
    | FetchYoutubePlaylists
    | GotSpotifyPlaylists (Result Http.Error String)
    | GotApplePlaylists (Result Http.Error String)
    | GotYoutubePlaylists (Result Http.Error String)
    | GotAppleUserToken String
    | ToggleAll Bool
    | ToggleOne String Bool
    | FetchLoginStatusAfterApple
    | AppleLoginAgain
    | TransferSelected
    | NoOp


trackDecoder : D.Decoder Track
trackDecoder =
    D.map3 Track
        (D.field "title" D.string)
        (D.field "artist" D.string)
        (D.field "isrc" (D.nullable D.string))


encodePlaylistItem : PlaylistItem -> E.Value
encodePlaylistItem p =
    E.object
        [ ( "id", E.string p.id )
        , ( "name", E.string p.name )
        , ( "cover", E.string p.cover )
        , ( "track_count", E.int p.trackCount )
        , ( "tracks"
          , E.list
                (\t ->
                    E.object
                        [ ( "title", E.string t.title )
                        , ( "artist", E.string t.artist )
                        , ( "isrc"
                          , case t.isrc of
                                Nothing ->
                                    E.null

                                Just i ->
                                    E.string i
                          )
                        ]
                )
                p.tracks
          )
        ]


sendToYoutube : PlaylistItem -> Cmd Msg
sendToYoutube p =
    Http.post
        { url = "/api/transfer/to/youtube"
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "playlist", encodePlaylistItem p ) ]
        , expect = Http.expectWhatever (\_ -> NoOp)
        }


sendToSpotify : PlaylistItem -> Cmd Msg
sendToSpotify p =
    Http.post
        { url = "/api/transfer/to/spotify"
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "playlist", encodePlaylistItem p ) ]
        , expect = Http.expectWhatever (\_ -> NoOp)
        }


sendToApple : PlaylistItem -> Cmd Msg
sendToApple p =
    Http.post
        { url = "/api/transfer/to/apple"
        , body =
            Http.jsonBody <| E.object [ ( "playlist", encodePlaylistItem p ) ]
        , expect = Http.expectWhatever (\_ -> NoOp)
        }


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( leftL, rightL, li ) =
            parseState url

        curFrom =
            List.Extra.getAt li leftL |> Maybe.withDefault Apple

        curTo =
            List.head rightL |> Maybe.withDefault Spotify

        model =
            { key = key
            , body = Home
            , loginStatuses = Dict.empty
            , leftList = leftL
            , rightList = rightL
            , leftIndex = li
            , currentFromType = curFrom
            , currentToType = curTo
            , from = serviceFromType curFrom
            , to = serviceFromType curTo
            , spotifyRaw = Nothing
            , appleRaw = Nothing
            , appleUserToken = Nothing
            , applePlaylists = []
            , spotifyPlaylists = []
            , youtubePlaylists = []
            , isLoading = False
            }
    in
    ( model
    , Cmd.batch
        [ fetchLoginStatus
        ]
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


decodePlaylistItem : D.Decoder PlaylistItem
decodePlaylistItem =
    D.map6 PlaylistItem
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "cover" D.string)
        (D.field "track_count" D.int)
        (D.succeed False)
        (D.field "tracks" (D.list trackDecoder))


decodeYoutubePlaylists : D.Decoder (List PlaylistItem)
decodeYoutubePlaylists =
    D.list decodePlaylistItem


decodeSpotifyPlaylists : D.Decoder (List PlaylistItem)
decodeSpotifyPlaylists =
    D.list decodePlaylistItem


decodeApplePlaylists : D.Decoder (List PlaylistItem)
decodeApplePlaylists =
    D.list decodePlaylistItem


viewRow : PlaylistItem -> Html Msg
viewRow p =
    div [ class "row" ]
        [ input [ type_ "checkbox", checked p.checked, onCheck (\b -> ToggleOne p.id b) ] []
        , div [ class "cell meta" ]
            [ img [ src p.cover, class "cover" ] []
            , div [ class "title" ] [ text p.name ]
            ]
        , div [ class "cell tracks" ]
            [ div [ class "track-list" ]
                (List.map
                    (\t -> div [ class "track" ] [ text (t.title ++ " - " ++ t.artist) ])
                    p.tracks
                )
            ]
        ]


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


serviceFromKey : String -> Maybe ServiceType
serviceFromKey k =
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


encodeList : List ServiceType -> String
encodeList =
    List.map serviceKey >> String.join ","


decodeList : String -> List ServiceType
decodeList s =
    s |> String.split "," |> List.filterMap serviceFromKey


encodeUrlFromModel : Model -> String
encodeUrlFromModel m =
    "/?left="
        ++ encodeList m.leftList
        ++ "&right="
        ++ encodeList m.rightList
        ++ "&li="
        ++ String.fromInt m.leftIndex


loginCmd : ServiceType -> Model -> Cmd Msg
loginCmd serviceType model =
    let
        stateStr =
            "left="
                ++ encodeList model.leftList
                ++ "&right="
                ++ encodeList model.rightList
                ++ "&li="
                ++ String.fromInt model.leftIndex

        encodedState =
            Url.percentEncode stateStr
    in
    case serviceType of
        Apple ->
            appleLogin ()

        Spotify ->
            Browser.Navigation.load
                ("https://accounts.spotify.com/authorize"
                    ++ "?client_id=a0e8851f25054913bffdfec463b47679"
                    ++ "&response_type=code"
                    ++ "&redirect_uri=https://replaylist.ngrok.io/api/login/spotify/callback"
                    ++ "&scope=playlist-read-private+playlist-modify-private"
                    ++ "&state="
                    ++ encodedState
                )

        Youtube ->
            Browser.Navigation.load
                ("https://accounts.google.com/o/oauth2/v2/auth"
                    ++ "?response_type=code"
                    ++ "&client_id="
                    ++ Url.percentEncode "263472270217-7ndt9q7oe9qm0r0dc01jaqu7p712a02h.apps.googleusercontent.com"
                    ++ "&redirect_uri="
                    ++ Url.percentEncode "https://replaylist.ngrok.io/api/login/youtube/callback"
                    ++ "&scope="
                    ++ Url.percentEncode "https://www.googleapis.com/auth/youtube.readonly"
                    ++ "&access_type=offline&include_granted_scopes=true&prompt=consent"
                    ++ "&state="
                    ++ encodedState
                )

        Amazon ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAppleUserToken token ->
            ( { model | appleUserToken = Just token }
            , Http.post
                { url = "/api/apple/usertoken"
                , body =
                    Http.jsonBody
                        (E.object
                            [ ( "token", E.string token ) ]
                        )
                , expect = Http.expectWhatever (\_ -> FetchLoginStatusAfterApple)
                }
            )

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
                ( leftL, rightL, li ) =
                    parseState url

                curFrom =
                    List.Extra.getAt li leftL |> Maybe.withDefault Apple

                curTo =
                    List.head rightL |> Maybe.withDefault Spotify
            in
            ( { model
                | from = serviceFromType curFrom
                , to = serviceFromType curTo
                , currentFromType = curFrom
                , currentToType = curTo
                , leftList = leftL
                , rightList = rightL
                , leftIndex = li
              }
            , fetchLoginStatus
            )

        FetchApplePlaylists ->
            ( { model | isLoading = True }
            , Http.get
                { url = "/api/apple/playlists"
                , expect = Http.expectString GotApplePlaylists
                }
            )

        FetchSpotifyPlaylists ->
            ( { model | isLoading = True }
            , Http.get
                { url = "/api/spotify/playlists"
                , expect = Http.expectString GotSpotifyPlaylists
                }
            )

        FetchYoutubePlaylists ->
            ( { model | isLoading = True }
            , Http.get
                { url = "/api/youtube/playlists"
                , expect = Http.expectString GotYoutubePlaylists
                }
            )

        GotSpotifyPlaylists (Ok raw) ->
            let
                decoded =
                    D.decodeString decodeSpotifyPlaylists raw
                        |> Result.withDefault []
            in
            ( { model | spotifyPlaylists = decoded, isLoading = False }, Cmd.none )

        GotSpotifyPlaylists (Err _) ->
            ( { model | spotifyRaw = Just "{\"error\":\"failed to fetch\"}", isLoading = False }, Cmd.none )

        GotApplePlaylists (Ok raw) ->
            let
                decoded =
                    D.decodeString decodeApplePlaylists raw
                        |> Result.withDefault []
            in
            ( { model | applePlaylists = decoded, isLoading = False }, Cmd.none )

        GotApplePlaylists (Err _) ->
            ( { model | appleRaw = Just "{\"error\":\"failed to fetch\"}", isLoading = False }, Cmd.none )

        GotYoutubePlaylists (Ok raw) ->
            let
                decoded =
                    D.decodeString decodeYoutubePlaylists raw
                        |> Result.withDefault []
            in
            ( { model | youtubePlaylists = decoded, isLoading = False }
            , Cmd.none
            )

        GotYoutubePlaylists (Err _) ->
            ( { model | isLoading = False }
            , Cmd.none
            )

        NextService ->
            let
                next =
                    min (model.leftIndex + 1) (List.length model.leftList - 1)

                newFromType =
                    List.Extra.getAt next model.leftList |> Maybe.withDefault model.currentFromType
            in
            ( { model
                | leftIndex = next
                , currentFromType = newFromType
                , from = serviceFromType newFromType
              }
            , Cmd.none
            )

        PrevService ->
            let
                prev =
                    max 0 (model.leftIndex - 1)

                newFromType =
                    List.Extra.getAt prev model.leftList |> Maybe.withDefault model.currentFromType
            in
            ( { model
                | leftIndex = prev
                , currentFromType = newFromType
                , from = serviceFromType newFromType
              }
            , Cmd.none
            )

        Swap ->
            case
                ( List.Extra.getAt model.leftIndex model.leftList
                , List.head model.rightList
                )
            of
                ( Just leftSel, Just rightHead ) ->
                    let
                        newLeft =
                            List.Extra.setAt model.leftIndex rightHead model.leftList

                        newRight =
                            leftSel :: List.drop 1 model.rightList

                        newFromType =
                            rightHead

                        newToType =
                            leftSel

                        newModel =
                            { model
                                | leftList = newLeft
                                , rightList = newRight
                                , currentFromType = newFromType
                                , currentToType = newToType
                                , from = serviceFromType newFromType
                                , to = serviceFromType newToType
                            }
                    in
                    ( newModel, Browser.Navigation.pushUrl model.key (encodeUrlFromModel newModel) )

                _ ->
                    ( model, Cmd.none )

        SendLogin serviceType ->
            case serviceType of
                Apple ->
                    ( model
                    , Cmd.batch
                        [ appleLogin ()
                        , Process.sleep 100 |> Task.perform (\_ -> AppleLoginAgain)
                        ]
                    )

                Spotify ->
                    ( model, loginCmd Spotify model )

                Youtube ->
                    ( model, loginCmd Youtube model )

                _ ->
                    ( model, Cmd.none )

        GoHome ->
            ( { model | body = Home }, Cmd.none )

        GoList ->
            let
                cmd =
                    if model.currentFromType == Spotify then
                        Task.perform (\_ -> FetchSpotifyPlaylists) (Task.succeed ())

                    else if model.currentFromType == Apple then
                        Task.perform (\_ -> FetchApplePlaylists) (Task.succeed ())

                    else if model.currentFromType == Youtube then
                        Task.perform (\_ -> FetchYoutubePlaylists) (Task.succeed ())

                    else
                        Cmd.none
            in
            ( { model | body = List, isLoading = True }, cmd )

        GoDone ->
            ( { model | body = Done }, Cmd.none )

        LogoutAll ->
            ( { model | loginStatuses = Dict.empty }
            , Http.post
                { url = "/api/logout_all"
                , body = Http.emptyBody
                , expect = Http.expectWhatever (\_ -> GotLoginStatus (Ok Dict.empty))
                }
            )

        ToggleAll state ->
            case model.currentFromType of
                Apple ->
                    ( { model
                        | applePlaylists = List.map (\p -> { p | checked = state }) model.applePlaylists
                      }
                    , Cmd.none
                    )

                Spotify ->
                    ( { model
                        | spotifyPlaylists = List.map (\p -> { p | checked = state }) model.spotifyPlaylists
                      }
                    , Cmd.none
                    )

                Youtube ->
                    ( { model
                        | youtubePlaylists = List.map (\p -> { p | checked = state }) model.youtubePlaylists
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleOne pid state ->
            case model.currentFromType of
                Apple ->
                    ( { model
                        | applePlaylists =
                            List.map
                                (\p ->
                                    if p.id == pid then
                                        { p | checked = state }

                                    else
                                        p
                                )
                                model.applePlaylists
                      }
                    , Cmd.none
                    )

                Spotify ->
                    ( { model
                        | spotifyPlaylists =
                            List.map
                                (\p ->
                                    if p.id == pid then
                                        { p | checked = state }

                                    else
                                        p
                                )
                                model.spotifyPlaylists
                      }
                    , Cmd.none
                    )

                Youtube ->
                    ( { model
                        | youtubePlaylists =
                            List.map
                                (\p ->
                                    if p.id == pid then
                                        { p | checked = state }

                                    else
                                        p
                                )
                                model.youtubePlaylists
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FetchLoginStatusAfterApple ->
            ( model, fetchLoginStatus )

        AppleLoginAgain ->
            ( model, appleLogin () )

        TransferSelected ->
            let
                selected =
                    case model.currentFromType of
                        Apple ->
                            List.filter .checked model.applePlaylists

                        Spotify ->
                            List.filter .checked model.spotifyPlaylists

                        Youtube ->
                            List.filter .checked model.youtubePlaylists

                        _ ->
                            []

                cmd =
                    case model.currentToType of
                        Spotify ->
                            Cmd.batch (List.map sendToSpotify selected)

                        Apple ->
                            Cmd.batch (List.map sendToApple selected)

                        Youtube ->
                            Cmd.batch (List.map sendToYoutube selected)

                        _ ->
                            Cmd.none
            in
            ( { model | body = Done }, cmd )

        NoOp ->
            ( model, Cmd.none )


serviceFromType : ServiceType -> Service
serviceFromType sType =
    case sType of
        Apple ->
            { name = "AppleMusic"
            , icon = "assets/AppleMusicIcon.png"
            , loginLabel = "Login with AppleMusic"
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


parseBody : Url -> Body
parseBody url =
    case url.path of
        "/list" ->
            List

        "/done" ->
            Done

        _ ->
            Home


parseState : Url -> ( List ServiceType, List ServiceType, Int )
parseState url =
    let
        leftS =
            Parser.parse (Parser.top <?> Query.string "left") url |> Maybe.withDefault Nothing

        rightS =
            Parser.parse (Parser.top <?> Query.string "right") url |> Maybe.withDefault Nothing

        liS =
            Parser.parse (Parser.top <?> Query.string "li") url |> Maybe.withDefault Nothing

        leftL =
            leftS |> Maybe.map decodeList |> Maybe.withDefault [ Youtube, Apple, Amazon ]

        rightL =
            rightS |> Maybe.map decodeList |> Maybe.withDefault [ Spotify ]

        li =
            liS |> Maybe.andThen String.toInt |> Maybe.withDefault 1
    in
    ( leftL, rightL, li )


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
                    [ leftCard model model.from model.currentFromType model.leftIndex model.leftList
                    , div [ class "swap-container" ]
                        [ button [ class "swap-btn", onClick Swap ] [ text "⇄" ] ]
                    , rightCard model model.to model.currentToType
                    ]
                ]

        List ->
            let
                currentList =
                    case model.currentFromType of
                        Apple ->
                            model.applePlaylists

                        Spotify ->
                            model.spotifyPlaylists

                        Youtube ->
                            model.youtubePlaylists

                        _ ->
                            []
            in
            viewPlaylistTable model.isLoading currentList

        Done ->
            div [] [ text "Hello, Body3", button [ onClick GoHome ] [ text "Back to Body1" ] ]


headerRow : Bool -> Html Msg
headerRow isLoading =
    div [ class "row playlist-header" ]
        [ input [ type_ "checkbox", onCheck ToggleAll ] []
        , div [ class "loading-row" ]
            [ if isLoading then
                div [ class "loading-bar" ] []

              else
                button
                    [ class "transfer-btn", onClick TransferSelected ]
                    [ text "Send Playlists →" ]
            ]
        ]


loadingIndicatorView : Html Msg
loadingIndicatorView =
    div [ class "loading-wrap" ]
        [ div [ class "loading-bar" ] [] ]


viewPlaylistTable : Bool -> List PlaylistItem -> Html Msg
viewPlaylistTable isLoading list =
    div [ class "playlist-table" ]
        (headerRow isLoading :: List.map viewRow list)


leftCard : Model -> Service -> ServiceType -> Int -> List ServiceType -> Html Msg
leftCard model service currentFromType currentFromIndex fromOptions =
    let
        disablePrev =
            currentFromIndex == 0

        disableNext =
            currentFromIndex == (List.length fromOptions - 1)

        isLoggedIn =
            Dict.get (serviceKey currentFromType) model.loginStatuses
                |> Maybe.withDefault False
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
        , button
            [ class "login-btn"
            , Html.Attributes.disabled isLoggedIn
            , onClick (SendLogin currentFromType)
            ]
            [ text service.loginLabel ]
        ]


rightCard : Model -> Service -> ServiceType -> Html Msg
rightCard model service currentToType =
    let
        isLoggedIn =
            Dict.get (serviceKey currentToType) model.loginStatuses
                |> Maybe.withDefault False
    in
    div [ class "card card-right" ]
        [ div [ class "card-title" ] [ text "TO:" ]
        , div [ class "card-icon" ]
            [ img [ src service.icon, class "music-icon" ] [] ]
        , div [ class "service-name-right" ] [ text service.name ]
        , button
            [ class "login-btn"
            , Html.Attributes.disabled isLoggedIn
            , onClick (SendLogin currentToType)
            ]
            [ text service.loginLabel ]
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


subscriptions model =
    receiveAppleUserToken GotAppleUserToken


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
