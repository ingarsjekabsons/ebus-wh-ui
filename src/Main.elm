module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h5, i, input, p, span, text)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type Msg
    = GetAllProducts
    | GotProducts (Result Http.Error (List Product))
    | ProductUpdated (Result Http.Error ())
    | ProductDeleted (Result Http.Error ())
    | EditProduct Product
    | CancelProductEdit
    | EditName String
    | EditDescription String
    | EditInStock String
    | CreateName String
    | CreateDescription String
    | CreateInStock String
    | SaveProduct Product
    | CreateProduct Product
    | CancelProductCreate
    | DeleteProduct Int


type alias Model =
    { products : List Product
    , editProd : Maybe Product
    , createProduct : Product
    , apiUrl : String
    }


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( { apiUrl = apiUrl, products = [], editProd = Nothing, createProduct = Product 0 "" "" 0 }
    , Http.get
        { url = apiUrl ++ "/products"
        , expect = Http.expectJson GotProducts (Decode.list prodDecoder)
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAllProducts ->
            ( model, Cmd.none )

        GotProducts res ->
            case res of
                Ok prods ->
                    ( { model | products = prods }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        EditProduct prod ->
            ( { model | editProd = Just prod }, Cmd.none )

        CancelProductEdit ->
            ( { model | editProd = Nothing }, Cmd.none )

        EditName name ->
            let
                newEdit =
                    Maybe.map (\p -> { p | name = name }) model.editProd
            in
            ( { model | editProd = newEdit }, Cmd.none )

        EditDescription descr ->
            let
                newEdit =
                    Maybe.map (\p -> { p | description = descr }) model.editProd
            in
            ( { model | editProd = newEdit }, Cmd.none )

        EditInStock i ->
            let
                newEdit =
                    Maybe.map (\p -> { p | inStock = String.toInt i |> Maybe.withDefault 0 }) model.editProd
            in
            ( { model | editProd = newEdit }, Cmd.none )

        CreateName name ->
            let
                old =
                    model.createProduct

                new =
                    { old | name = name }
            in
            ( { model | createProduct = new }, Cmd.none )

        CreateDescription descr ->
            let
                old =
                    model.createProduct

                new =
                    { old | description = descr }
            in
            ( { model | createProduct = new }, Cmd.none )

        CreateInStock i ->
            let
                old =
                    model.createProduct

                new =
                    { old | inStock = Maybe.withDefault 0 (String.toInt i) }
            in
            ( { model | createProduct = new }, Cmd.none )

        SaveProduct product ->
            ( model
            , Http.request
                { url = model.apiUrl ++ "/products/" ++ String.fromInt product.id
                , expect = Http.expectWhatever ProductUpdated
                , method = "PATCH"
                , body = Http.jsonBody (prodEncoder product)
                , headers = []
                , timeout = Just 60
                , tracker = Nothing
                }
            )

        CreateProduct product ->
            ( { model | createProduct = Product 0 "" "" 0 }
            , Http.post
                { url = model.apiUrl ++ "/products"
                , expect = Http.expectWhatever ProductUpdated
                , body = Http.jsonBody (prodEncoder product)
                }
            )

        ProductUpdated res ->
            case res of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( model
                    , Http.get
                        { url = model.apiUrl ++ "/products"
                        , expect = Http.expectJson GotProducts (Decode.list prodDecoder)
                        }
                    )

        ProductDeleted res ->
            case res of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( model
                    , Http.get
                        { url = model.apiUrl ++ "/products"
                        , expect = Http.expectJson GotProducts (Decode.list prodDecoder)
                        }
                    )

        CancelProductCreate ->
            ( { model | createProduct = Product 0 "" "" 0 }, Cmd.none )

        DeleteProduct i ->
            ( model
            , Http.request
                { url = model.apiUrl ++ "/products/" ++ String.fromInt i
                , expect = Http.expectWhatever ProductDeleted
                , method = "DELETE"
                , body = Http.emptyBody
                , headers = []
                , timeout = Just 60
                , tracker = Nothing
                }
            )


view : Model -> Html Msg
view model =
    let
        pc =
            model.createProduct
    in
    div [ Attrs.class "container-fluid" ]
        [ div [ Attrs.class "row" ]
            [ div [ Attrs.class "col-sm-4" ]
                [ div [ Attrs.class "h3", Attrs.class "text-center" ] [ text "Warehouse products" ]
                , div [] []
                ]
            ]
        , div [ Attrs.class "row" ]
            [ div [ Attrs.class "col-sm-6" ]
                [ button
                    [ Attrs.type_ "button"
                    , Attrs.class "btn"
                    , Attrs.class "btn-primary"
                    , Attrs.class "float-sm-end"
                    , Attrs.attribute "data-bs-toggle" "modal"
                    , Attrs.attribute "data-bs-target" "#createProduct"
                    ]
                    [ text "Add Product" ]
                ]
            ]
        , div [ Attrs.class "row" ]
            [ div [ Attrs.class "col-sm-12" ]
                [ div [ Attrs.class "modal", Attrs.attribute "tabindex" "-1", Attrs.id "createProduct" ]
                    [ div [ Attrs.class "modal-dialog" ]
                        [ div [ Attrs.class "modal-content" ]
                            [ div [ Attrs.class "modal-header" ]
                                [ h5 [ Attrs.class "modal-title" ] [ text "Create product" ]
                                , button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn-close"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Attrs.attribute "aria-label" "Close"
                                    , Events.onClick CancelProductCreate
                                    ]
                                    []
                                ]
                            , div [ Attrs.class "modal-body" ]
                                [ div []
                                    [ div []
                                        [ div [ Attrs.class "row" ]
                                            [ text "Name: "
                                            , input
                                                [ Attrs.type_ "text"
                                                , Attrs.value pc.name
                                                , Attrs.placeholder "name"
                                                , Events.onInput CreateName
                                                ]
                                                []
                                            ]
                                        , div [ Attrs.class "row" ]
                                            [ text "Description: "
                                            , input
                                                [ Attrs.type_ "text"
                                                , Attrs.value pc.description
                                                , Attrs.placeholder "description"
                                                , Events.onInput CreateDescription
                                                ]
                                                []
                                            ]
                                        , div [ Attrs.class "row" ]
                                            [ text "Available in stock: "
                                            , input
                                                [ Attrs.type_ "text"
                                                , Attrs.value (String.fromInt pc.inStock)
                                                , Attrs.placeholder "In stock"
                                                , Events.onInput CreateInStock
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ Attrs.class "modal-footer" ]
                                [ button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn"
                                    , Attrs.class "btn-secondary"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Events.onClick CancelProductCreate
                                    ]
                                    [ text "Close" ]
                                , button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn"
                                    , Attrs.class "btn-primary"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Events.onClick (CreateProduct model.createProduct)
                                    ]
                                    [ text "Create" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ Attrs.class "row" ]
            [ div [ Attrs.class "col-sm-12" ]
                [ div [ Attrs.class "modal", Attrs.attribute "tabindex" "-1", Attrs.id "editProduct" ]
                    [ div [ Attrs.class "modal-dialog" ]
                        [ div [ Attrs.class "modal-content" ]
                            [ div [ Attrs.class "modal-header" ]
                                [ h5 [ Attrs.class "modal-title" ] [ text "Edit product" ]
                                , button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn-close"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Attrs.attribute "aria-label" "Close"
                                    , Events.onClick CancelProductEdit
                                    ]
                                    []
                                ]
                            , div [ Attrs.class "modal-body" ]
                                [ div []
                                    [ case model.editProd of
                                        Nothing ->
                                            text "No product selected for editing"

                                        Just p ->
                                            div []
                                                [ div [ Attrs.class "row" ]
                                                    [ text "Name: "
                                                    , input
                                                        [ Attrs.type_ "text"
                                                        , Attrs.value p.name
                                                        , Attrs.placeholder "name"
                                                        , Events.onInput EditName
                                                        ]
                                                        []
                                                    ]
                                                , div [ Attrs.class "row" ]
                                                    [ text "Description: "
                                                    , input
                                                        [ Attrs.type_ "text"
                                                        , Attrs.value p.description
                                                        , Attrs.placeholder "description"
                                                        , Events.onInput EditDescription
                                                        ]
                                                        []
                                                    ]
                                                , div [ Attrs.class "row" ]
                                                    [ text "Available in stock: "
                                                    , input
                                                        [ Attrs.type_ "text"
                                                        , Attrs.value (String.fromInt p.inStock)
                                                        , Attrs.placeholder "In stock"
                                                        , Events.onInput EditInStock
                                                        ]
                                                        []
                                                    ]
                                                ]
                                    ]
                                ]
                            , div [ Attrs.class "modal-footer" ]
                                [ button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn"
                                    , Attrs.class "btn-secondary"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Events.onClick CancelProductEdit
                                    ]
                                    [ text "Close" ]
                                , button
                                    [ Attrs.type_ "button"
                                    , Attrs.class "btn"
                                    , Attrs.class "btn-primary"
                                    , Attrs.attribute "data-bs-dismiss" "modal"
                                    , Events.onClick (SaveProduct (Maybe.withDefault (Product 1 "a" "a" 0) model.editProd))
                                    ]
                                    [ text "Save changes" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ Attrs.class "row" ]
            [ div [ Attrs.class "col-sm-3", Attrs.class "d-grip", Attrs.class "gap-3" ]
                [ div []
                    (List.map
                        (\prod ->
                            div [ Attrs.class "card", Attrs.class "p-2", Attrs.class "mb-sm-3", Attrs.class "text-bg-dark" ]
                                [ h5 [ Attrs.class "card-title" ]
                                    [ text (prod.name ++ " ")
                                    , a
                                        [ Attrs.href "#"
                                        , Events.onClick (EditProduct prod)
                                        , Attrs.attribute "data-bs-toggle" "modal"
                                        , Attrs.attribute "data-bs-target" "#editProduct"
                                        ]
                                        [ i
                                            [ Attrs.class "bi"
                                            , Attrs.class "bi-pen"
                                            , Attrs.class "text-muted"
                                            , Attrs.style "font-size" "14px"
                                            ]
                                            []
                                        ]
                                    , div [ Attrs.class "float-end", Attrs.class "text-bg-danger" ]
                                        [ a
                                            [ Attrs.href "#"
                                            , Attrs.class "float-end"
                                            ]
                                            [ i
                                                [ Attrs.class "bi"
                                                , Attrs.class "bi-x-square-fill"
                                                , Events.onClick (DeleteProduct prod.id)
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                , p [ Attrs.class "card-body" ] [ text prod.description ]
                                , div [ Attrs.class "col-sm-8", Attrs.class "text-right" ]
                                    [ div [ Attrs.class "row" ]
                                        [ div [ Attrs.class "col-sm-6" ] [ text "Available in stock: " ]
                                        , div [ Attrs.class "col-sm-1" ]
                                            [ span
                                                [ Attrs.class "bg-info"
                                                , Attrs.class "text-dark"
                                                , Attrs.class "badge"
                                                ]
                                                [ text (String.fromInt prod.inStock) ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        model.products
                    )
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Product =
    { id : Int
    , name : String
    , description : String
    , inStock : Int
    }


prodDecoder : Decode.Decoder Product
prodDecoder =
    Decode.map4 Product
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "in_stock" Decode.int)


prodEncoder : Product -> Encode.Value
prodEncoder prod =
    Encode.object
        [ ( "id", Encode.int prod.id )
        , ( "name", Encode.string prod.name )
        , ( "description", Encode.string prod.description )
        , ( "in_stock", Encode.int prod.inStock )
        ]


httpError : Http.Error -> String
httpError e =
    case e of
        Http.BadUrl s ->
            "bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus i ->
            "Bad status: " ++ String.fromInt i

        Http.BadBody s ->
            "Bad body: " ++ s
