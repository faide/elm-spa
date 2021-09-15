module Spa.PageStack exposing (CurrentViewMap, Model, Msg, PreviousViewMap, Stack, add, empty, hasNoCurrentPage, setup)

import Effect exposing (Effect)
import Internal exposing (Page(..))


type Model current previous
    = NoPage
    | Current current
    | Previous previous


type Msg current previous
    = CurrentMsg current
    | PreviousMsg previous


type alias Stack shared sharedMsg route view current previous msg =
    { init : shared -> route -> ( Model current previous, Effect sharedMsg msg )
    , update : shared -> msg -> Model current previous -> ( Model current previous, Effect sharedMsg msg )
    , subscriptions : shared -> Model current previous -> Sub msg
    , view : shared -> Model current previous -> view
    }


type alias PageSetup flags shared sharedMsg view model msg =
    shared -> Maybe (Page flags sharedMsg view model msg)


type alias RouteMatcher route flags =
    route -> Maybe flags


empty : Model a b
empty =
    NoPage


hasNoCurrentPage : Model a b -> Bool
hasNoCurrentPage =
    (==) NoPage


mapPrevious : Model pa pb -> Model a (Model pa pb)
mapPrevious m =
    case m of
        NoPage ->
            NoPage

        _ ->
            Previous m


setup :
    { defaultView : view
    }
    -> Stack shared sharedMsg route view () () ()
setup { defaultView } =
    { init = \_ _ -> ( NoPage, Effect.none )
    , update = \_ _ _ -> ( NoPage, Effect.none )
    , view = \_ _ -> defaultView
    , subscriptions = \_ _ -> Sub.none
    }


init : Model current previous
init =
    NoPage


type alias CurrentViewMap currentMsg previousMsg pageView view =
    (currentMsg -> Msg currentMsg previousMsg) -> pageView -> view


type alias PreviousViewMap currentMsg previousMsg previousView view =
    (previousMsg -> Msg currentMsg previousMsg) -> previousView -> view


add :
    ( CurrentViewMap currentMsg previousMsg pageView view
    , PreviousViewMap currentMsg previousMsg previousView view
    )
    -> RouteMatcher route flags
    -> PageSetup flags shared sharedMsg pageView pageModel pageMsg
    -> Stack shared sharedMsg route previousView previousCurrent previousPrevious previousMsg
    -> Stack shared sharedMsg route view pageModel (Model previousCurrent previousPrevious) (Msg pageMsg previousMsg)
add ( mapPageView, mapPreviousView ) match pagesetup previousStack =
    { init =
        \shared route ->
            case match route of
                Just flags ->
                    case pagesetup shared of
                        Just (Page page) ->
                            let
                                ( pageModel, pageEffect ) =
                                    page.init flags
                            in
                            ( Current pageModel
                            , Effect.map CurrentMsg pageEffect
                            )

                        Nothing ->
                            ( NoPage, Effect.none )

                Nothing ->
                    let
                        ( prevModel, prevEffect ) =
                            previousStack.init shared route
                    in
                    ( mapPrevious prevModel
                    , Effect.map PreviousMsg prevEffect
                    )
    , update =
        \shared msg model ->
            case ( msg, model ) of
                ( CurrentMsg pageMsg, Current pageModel ) ->
                    case pagesetup shared of
                        Just (Page page) ->
                            let
                                ( newPageModel, newPageEffect ) =
                                    page.update pageMsg pageModel
                            in
                            ( Current newPageModel, Effect.map CurrentMsg newPageEffect )

                        Nothing ->
                            ( NoPage, Effect.none )

                ( PreviousMsg previousMsg, Previous previousModel ) ->
                    let
                        ( newPreviousModel, previousEffect ) =
                            previousStack.update shared previousMsg previousModel
                    in
                    ( mapPrevious newPreviousModel, Effect.map PreviousMsg previousEffect )

                ( _, NoPage ) ->
                    ( model, Effect.none )

                ( CurrentMsg pageMsg, _ ) ->
                    ( model, Effect.none )

                ( PreviousMsg prevMsg, _ ) ->
                    ( model, Effect.none )
    , subscriptions =
        \shared model ->
            case model of
                NoPage ->
                    Sub.none

                Current pageModel ->
                    case pagesetup shared of
                        Just (Page page) ->
                            page.subscriptions pageModel
                                |> Sub.map CurrentMsg

                        Nothing ->
                            Sub.none

                Previous prevModel ->
                    previousStack.subscriptions shared prevModel
                        |> Sub.map PreviousMsg
    , view =
        \shared model ->
            case model of
                Current pageModel ->
                    case pagesetup shared of
                        Just (Page page) ->
                            page.view pageModel
                                |> mapPageView CurrentMsg

                        Nothing ->
                            previousStack.view shared NoPage
                                |> mapPreviousView PreviousMsg

                Previous previousModel ->
                    previousStack.view shared previousModel
                        |> mapPreviousView PreviousMsg

                NoPage ->
                    previousStack.view shared NoPage
                        |> mapPreviousView PreviousMsg
    }
