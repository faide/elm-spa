module Spa exposing
    ( init, initNoShared
    , addPublicPage, addProtectedPage
    , application
    )

{-|


# Create the application

@docs init, initNoShared


# Add pages

@docs addPublicPage, addProtectedPage


# Finalize

Once all the pages are added to the application, we can change it into a record
suitable for the `Browser.application` function.

@docs application

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html
import Internal exposing (PageDefinition)
import Spa.PageStack as PageStack
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser)


type Msg sharedMsg pageMsg
    = SharedMsg sharedMsg
    | PageMsg pageMsg
    | UrlRequest UrlRequest
    | UrlChange Url


type alias Page flags sharedMsg view model msg =
    Internal.Page flags sharedMsg view model msg


type PageSetup flags identity shared sharedMsg view model msg
    = PublicPage (shared -> Page flags sharedMsg view model msg)
    | ProtectedPage (shared -> identity -> Page flags sharedMsg view model msg)


type alias Model route shared current previous =
    { key : Nav.Key
    , currentRoute : route
    , shared : shared
    , page : PageStack.Model current previous
    }


modelShared : Model route shared current previous -> shared
modelShared { shared } =
    shared


initModel : route -> Nav.Key -> shared -> Model route shared current previous
initModel route key shared =
    { key = key
    , currentRoute = route
    , shared = shared
    , page = PageStack.empty
    }


type alias Builder route identity shared sharedMsg view current previous stackMsg =
    { toRoute : Url -> route
    , extractIdentity : shared -> Maybe identity
    , pageStack : PageStack.Stack shared sharedMsg route view current previous stackMsg
    }


{-| Bootstrap a Spa application

    Spa.init
        { init = Shared.init
        , subscriptions = Shared.subscriptions
        , update = Shared.update
        , defaultView = View.defaultView
        , toRoute = Route.toRoute
        , extractIdentity = Shared.identity
        , protectPage = Route.toUrl >> Just >> Route.SignIn >> Route.toUrl
        }

-}
init :
    { defaultView : view
    , toRoute : Url -> route
    , extractIdentity : shared -> Maybe identity
    }
    -> Builder route identity shared sharedMsg view () () ()
init shared =
    { toRoute = shared.toRoute
    , extractIdentity = shared.extractIdentity
    , pageStack = PageStack.setup { defaultView = shared.defaultView }
    }


{-| Bootstrap a Spa application that has no Shared state
-}
initNoShared :
    { defaultView : view
    , toRoute : Url -> route
    }
    -> Builder route () () () view () () ()
initNoShared { toRoute, defaultView } =
    init
        { defaultView = defaultView
        , toRoute = toRoute
        , extractIdentity = always Nothing
        }


builderInit :
    (Url -> route)
    -> (flags -> Nav.Key -> ( shared, Cmd sharedMsg ))
    -> flags
    -> Url
    -> Nav.Key
    -> ( Model route shared () (), Cmd (Msg sharedMsg ()) )
builderInit toRoute sharedInit flags url key =
    let
        ( shared, sharedCmd ) =
            sharedInit flags key
    in
    ( initModel (toRoute url) key shared, Cmd.map SharedMsg sharedCmd )


setupPage :
    (shared -> Maybe identity)
    -> shared
    -> PageSetup pageFlags identity shared sharedMsg pageView currentPageModel currentPageMsg
    -> Maybe (Page pageFlags sharedMsg pageView currentPageModel currentPageMsg)
setupPage extractIdentity shared page =
    case page of
        PublicPage setup ->
            Just <| setup shared

        ProtectedPage setup ->
            extractIdentity shared
                |> Maybe.map (setup shared)


{-| Add a public page to the application
-}
addPublicPage :
    ( PageStack.CurrentViewMap currentPageMsg previousStackMsg pageView view
    , PageStack.PreviousViewMap currentPageMsg previousStackMsg previousView view
    )
    -> (route -> Maybe pageFlags)
    -> (shared -> Page pageFlags sharedMsg pageView currentPageModel currentPageMsg)
    -> Builder route identity shared sharedMsg previousView previousCurrent previousPrevious previousStackMsg
    -> Builder route identity shared sharedMsg view currentPageModel (PageStack.Model previousCurrent previousPrevious) (PageStack.Msg currentPageMsg previousStackMsg)
addPublicPage mappers matchRoute page =
    addPage mappers matchRoute (PublicPage page)


{-| Add a protected page to the application
-}
addProtectedPage :
    ( PageStack.CurrentViewMap currentPageMsg previousStackMsg pageView view
    , PageStack.PreviousViewMap currentPageMsg previousStackMsg previousView view
    )
    -> (route -> Maybe pageFlags)
    -> (shared -> identity -> Page pageFlags sharedMsg pageView currentPageModel currentPageMsg)
    -> Builder route identity shared sharedMsg previousView previousCurrent previousPrevious previousStackMsg
    -> Builder route identity shared sharedMsg view currentPageModel (PageStack.Model previousCurrent previousPrevious) (PageStack.Msg currentPageMsg previousStackMsg)
addProtectedPage mappers matchRoute page =
    addPage mappers matchRoute (ProtectedPage page)


addPage :
    ( PageStack.CurrentViewMap currentPageMsg previousStackMsg pageView view
    , PageStack.PreviousViewMap currentPageMsg previousStackMsg previousView view
    )
    -> (route -> Maybe pageFlags)
    -> PageSetup pageFlags identity shared sharedMsg pageView currentPageModel currentPageMsg
    -> Builder route identity shared sharedMsg previousView prev prevprev previousStackMsg
    -> Builder route identity shared sharedMsg view currentPageModel (PageStack.Model prev prevprev) (PageStack.Msg currentPageMsg previousStackMsg)
addPage mappers matchRoute page builder =
    let
        pageStack =
            builder.pageStack
                |> PageStack.add mappers
                    matchRoute
                    (\shared ->
                        setupPage builder.extractIdentity
                            shared
                            page
                    )
    in
    { toRoute = builder.toRoute
    , extractIdentity = builder.extractIdentity
    , pageStack = pageStack
    }


{-| Finalize the Spa application into a record suitable for the `Browser.application`

`toDocument` is a function that convert a view to a `Browser.Document`

    appWithPages
        |> Spa.application { toDocument = View.toDocument }
        |> Browser.application

-}
application :
    ((pageMsg -> Msg sharedMsg pageMsg) -> pageView -> view)
    ->
        { init : flags -> Nav.Key -> ( shared, Cmd sharedMsg )
        , subscriptions : shared -> Sub sharedMsg
        , update : sharedMsg -> shared -> ( shared, Cmd sharedMsg )
        , protectPage : route -> String
        , toDocument : shared -> view -> Document (Msg sharedMsg pageMsg)
        }
    -> Builder route identity shared sharedMsg pageView current previous pageMsg
    ->
        { init : flags -> Url -> Nav.Key -> ( Model route shared current previous, Cmd (Msg sharedMsg pageMsg) )
        , view : Model route shared current previous -> Document (Msg sharedMsg pageMsg)
        , update : Msg sharedMsg pageMsg -> Model route shared current previous -> ( Model route shared current previous, Cmd (Msg sharedMsg pageMsg) )
        , subscriptions : Model route shared current previous -> Sub (Msg sharedMsg pageMsg)
        , onUrlRequest : UrlRequest -> Msg sharedMsg pageMsg
        , onUrlChange : Url -> Msg sharedMsg pageMsg
        }
application viewMap app builder =
    let
        initPage route key shared =
            let
                ( page, effect ) =
                    builder.pageStack.init shared route
            in
            if
                page
                    |> Debug.log "page"
                    |> PageStack.hasNoCurrentPage
                    |> Debug.log "hasNoCurrentPage"
            then
                ( PageStack.empty, Nav.replaceUrl key <| app.protectPage route )

            else
                ( page, Effect.toCmd ( SharedMsg, PageMsg ) effect )
    in
    { init =
        \flags url key ->
            let
                route =
                    builder.toRoute url

                ( shared, sharedCmd ) =
                    app.init flags key

                ( page, pageCmd ) =
                    initPage route key shared
            in
            ( { key = key
              , currentRoute = route
              , shared = shared
              , page = page
              }
            , Cmd.batch
                [ Cmd.map SharedMsg sharedCmd
                , pageCmd
                ]
            )
    , view =
        \model ->
            builder.pageStack.view model.shared model.page
                |> viewMap PageMsg
                |> app.toDocument (modelShared model)
    , update =
        \msg model ->
            case msg of
                SharedMsg sharedMsg ->
                    let
                        ( newShared, sharedCmd ) =
                            app.update sharedMsg model.shared
                    in
                    ( { model | shared = newShared }
                    , Cmd.map SharedMsg sharedCmd
                    )

                PageMsg pageMsg ->
                    let
                        ( newPage, pageEffect ) =
                            builder.pageStack.update model.shared pageMsg model.page
                    in
                    ( { model | page = newPage }
                    , Effect.toCmd ( SharedMsg, PageMsg ) pageEffect
                    )

                UrlRequest urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( model
                            , Nav.pushUrl model.key (Url.toString url)
                            )

                        Browser.External url ->
                            ( model
                            , Nav.load url
                            )

                UrlChange url ->
                    let
                        route =
                            builder.toRoute url

                        ( page, pageCmd ) =
                            initPage route model.key model.shared
                    in
                    ( { model
                        | currentRoute = route
                        , page = page
                      }
                    , pageCmd
                    )
    , subscriptions =
        \model ->
            Sub.batch
                [ app.subscriptions model.shared |> Sub.map SharedMsg
                , builder.pageStack.subscriptions model.shared model.page |> Sub.map PageMsg
                ]
    , onUrlRequest = UrlRequest
    , onUrlChange = UrlChange
    }
