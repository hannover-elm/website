module Logo exposing (main)

import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Illuminance exposing (Illuminance)
import Json.Decode as Decode
import Length exposing (Length, Meters)
import Pixels
import Plane3d
import Point2d
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import SketchPlane3d
import Sphere3d
import Task exposing (Task)
import Triangle2d
import Triangle3d
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)


displayControls : Bool
displayControls =
    False


margin : Float
margin =
    8


aspectRatio : Float
aspectRatio =
    385 / 600


type Model
    = Loading
    | Loaded Viewport LoadedData


type alias LoadedData =
    { focalPointZ : Float
    , eyePointY : Float
    , eyePointZ : Float
    , verticalFieldOfView : Float
    , timeElapsed : Duration
    , materials : MaterialInputs
    , light : LightInput
    , cachedBuilding : Entity Xyz
    }


type Xyz
    = Xyz


type alias MaterialInputs =
    { brick : MaterialInput
    , shingles : MaterialInput
    , copper : MaterialInput
    , grass : MaterialInput
    }


initialLoadedData : LoadedData
initialLoadedData =
    let
        materials =
            { brick =
                { baseColor = Color.hsl 0.11 0.57 0.78
                , roughness = 1
                }
            , shingles =
                { baseColor = Color.hsl 0.04 1 0.38
                , roughness = 0.61
                }
            , copper =
                { baseColor = Color.hsl 0.44 0.36 0.72
                , roughness = 0.76
                }
            , grass =
                { baseColor = Color.hsl 0.27 0.5 0.34
                , roughness = 0.89
                }
            }
    in
    { focalPointZ = 3.01
    , eyePointY = -21.5
    , eyePointZ = 0.75
    , verticalFieldOfView = 20
    , timeElapsed = Duration.seconds 0
    , light =
        { azimuth = Angle.degrees 60
        , elevation = Angle.degrees -32.5
        }
    , materials = materials
    , cachedBuilding = building materials
    }


viewpoint : LoadedData -> Viewpoint3d Meters world
viewpoint ({ timeElapsed } as loadedData) =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.meters 0 0 loadedData.focalPointZ
        , eyePoint =
            Point3d.meters 0
                (loadedData.eyePointY - 0.25 * cos (Duration.inSeconds timeElapsed / 3))
                (loadedData.eyePointZ + 0.45 * cos (Duration.inSeconds timeElapsed / 5))
        , upDirection = Direction3d.positiveZ
        }


camera : LoadedData -> Camera3d Meters world
camera loadedData =
    Camera3d.perspective
        { viewpoint = viewpoint loadedData
        , verticalFieldOfView = Angle.degrees loadedData.verticalFieldOfView
        }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = FocalPointZChanged Float
    | EyePointYChanged Float
    | EyePointZChanged Float
    | VerticalFieldOfViewChanged Float
    | Simulated Float
    | BrickChanged MaterialInput
    | ShinglesChanged MaterialInput
    | CopperChanged MaterialInput
    | GrassChanged MaterialInput
    | LightChanged LightInput
    | Resized Viewport


type alias MaterialInput =
    { baseColor : Color
    , roughness : Float
    }


type alias LightInput =
    { azimuth : Angle
    , elevation : Angle
    }


type alias Viewport =
    { width : Float, height : Float }


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading
    , Browser.Dom.getViewport
        |> Task.map (\viewport -> viewport.scene)
        |> Task.perform Resized
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Resized viewport, Loading ) ->
            ( Loaded viewport initialLoadedData, Cmd.none )

        ( _, Loading ) ->
            ( Loading, Cmd.none )

        ( Resized viewport, Loaded _ loadedData ) ->
            ( Loaded viewport (updateLoaded msg loadedData), Cmd.none )

        ( _, Loaded viewport loadedData ) ->
            ( Loaded viewport (updateLoaded msg loadedData), Cmd.none )


updateLoaded : Msg -> LoadedData -> LoadedData
updateLoaded msg loadedData =
    case msg of
        FocalPointZChanged focalPointZ ->
            { loadedData | focalPointZ = focalPointZ }

        EyePointYChanged eyePointY ->
            { loadedData | eyePointY = eyePointY }

        EyePointZChanged eyePointZ ->
            { loadedData | eyePointZ = eyePointZ }

        VerticalFieldOfViewChanged verticalFieldOfView ->
            { loadedData | verticalFieldOfView = verticalFieldOfView }

        BrickChanged brick ->
            mapMaterials (\materials -> { materials | brick = brick }) loadedData
                |> recomputeCachedBuilding

        ShinglesChanged shingles ->
            mapMaterials (\materials -> { materials | shingles = shingles }) loadedData
                |> recomputeCachedBuilding

        CopperChanged copper ->
            mapMaterials (\materials -> { materials | copper = copper }) loadedData
                |> recomputeCachedBuilding

        GrassChanged grass ->
            mapMaterials (\materials -> { materials | grass = grass }) loadedData
                |> recomputeCachedBuilding

        LightChanged light ->
            { loadedData | light = light }

        Simulated timeSinceLastFrame ->
            { loadedData
                | timeElapsed =
                    Quantity.plus (Duration.milliseconds timeSinceLastFrame)
                        loadedData.timeElapsed
            }

        Resized _ ->
            loadedData


mapMaterials : (MaterialInputs -> MaterialInputs) -> LoadedData -> LoadedData
mapMaterials f loadedData =
    { loadedData | materials = f loadedData.materials }


recomputeCachedBuilding : LoadedData -> LoadedData
recomputeCachedBuilding ({ materials } as loadedData) =
    { loadedData | cachedBuilding = building materials }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        onResized =
            Browser.Events.onResize
                (\width height ->
                    Resized { width = toFloat width, height = toFloat height }
                )

        onSimulated =
            Browser.Events.onAnimationFrameDelta Simulated
    in
    case model of
        Loading ->
            onResized

        Loaded _ _ ->
            Sub.batch [ onResized, onSimulated ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text ""

        Loaded viewport loadedData ->
            Html.div []
                [ scene viewport loadedData
                , controls viewport loadedData
                ]


controls : Viewport -> LoadedData -> Html Msg
controls viewport loadedData =
    Html.div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "640px"
        , style "width" "280px"
        , style "max-height" "100vh"
        , style "overflow-y" "auto"
        , style "padding-right" "20px"
        , style "display"
            (if displayControls then
                "block"

             else
                "none"
            )
        ]
        [ slider
            { label = "focalPointZ"
            , onChange = FocalPointZChanged
            , min = -10
            , max = 10
            , value = loadedData.focalPointZ
            }
        , slider
            { label = "eyePointY"
            , onChange = EyePointYChanged
            , min = -20
            , max = 5
            , value = loadedData.eyePointY
            }
        , slider
            { label = "eyePointZ"
            , onChange = EyePointZChanged
            , min = -10
            , max = 30
            , value = loadedData.eyePointZ
            }
        , slider
            { label = "verticalFieldOfView"
            , onChange = VerticalFieldOfViewChanged
            , min = 0
            , max = 100
            , value = loadedData.verticalFieldOfView
            }
        , slider
            { label = "timeElapsed"
            , onChange = Simulated
            , min = 0
            , max = 9999
            , value = Duration.inSeconds loadedData.timeElapsed
            }
        , lightControl
            { label = "key"
            , onChange = LightChanged
            , value = loadedData.light
            }
        , materialControl
            { label = "brick"
            , onChange = BrickChanged
            , value = loadedData.materials.brick
            }
        , materialControl
            { label = "shingles"
            , onChange = ShinglesChanged
            , value = loadedData.materials.shingles
            }
        , materialControl
            { label = "copper"
            , onChange = CopperChanged
            , value = loadedData.materials.copper
            }
        , materialControl
            { label = "grass"
            , onChange = GrassChanged
            , value = loadedData.materials.grass
            }
        ]


slider :
    { label : String
    , onChange : Float -> msg
    , min : Float
    , max : Float
    , value : Float
    }
    -> Html msg
slider { label, onChange, min, max, value } =
    let
        resolution =
            100
    in
    Html.label []
        [ Html.span [ style "display" "block" ]
            [ text (label ++ ": " ++ String.fromFloat value)
            ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max (String.fromFloat (resolution * (max - min)))
            , Html.Attributes.value (String.fromFloat (resolution * (value - min)))
            , Html.Events.on "input"
                (Decode.map
                    (onChange
                        << Maybe.withDefault value
                        << Maybe.map (\value_ -> min + value_ / resolution)
                        << String.toFloat
                    )
                    Html.Events.targetValue
                )
            , style "display" "block"
            ]
            []
        ]


materialControl :
    { label : String
    , onChange : MaterialInput -> msg
    , value : MaterialInput
    }
    -> Html msg
materialControl { label, onChange, value } =
    let
        { hue, saturation, lightness } =
            Color.toHsla value.baseColor

        { roughness } =
            value

        fromHue newHue =
            { value | baseColor = Color.hsl newHue saturation lightness }

        fromSaturation newSaturation =
            { value | baseColor = Color.hsl hue newSaturation lightness }

        fromLightness newLightness =
            { value | baseColor = Color.hsl hue saturation newLightness }

        fromRoughness newRoughness =
            { value | roughness = newRoughness }
    in
    Html.div []
        [ Html.label [] [ text (label ++ ":") ]
        , slider
            { label = "Hue"
            , onChange = onChange << fromHue
            , min = 0
            , max = 1
            , value = hue
            }
        , slider
            { label = "Saturation"
            , onChange = onChange << fromSaturation
            , min = 0
            , max = 1
            , value = saturation
            }
        , slider
            { label = "Luminance"
            , onChange = onChange << fromLightness
            , min = 0
            , max = 1
            , value = lightness
            }
        , slider
            { label = "Roughness"
            , onChange = onChange << fromRoughness
            , min = 0
            , max = 1
            , value = roughness
            }
        ]


lightControl :
    { label : String
    , onChange : LightInput -> msg
    , value : LightInput
    }
    -> Html msg
lightControl { label, onChange, value } =
    let
        { azimuth, elevation } =
            value
    in
    Html.div []
        [ Html.label [] [ text (label ++ ":") ]
        , slider
            { label = "azimuth"
            , onChange =
                \newAzimuth -> onChange { value | azimuth = Angle.degrees newAzimuth }
            , min = 0
            , max = 360
            , value = Angle.inDegrees azimuth
            }
        , slider
            { label = "elevation"
            , onChange =
                \newElevation ->
                    onChange { value | elevation = Angle.degrees newElevation }
            , min = -90
            , max = 90
            , value = Angle.inDegrees elevation
            }
        ]


scene : Viewport -> LoadedData -> Html Msg
scene viewport ({ light, timeElapsed } as loadedData) =
    let
        width =
            floor (min 600 (viewport.width - 2 * margin))

        height =
            round (toFloat width * aspectRatio)
    in
    Scene3d.sunny
        { upDirection = Direction3d.positiveZ
        , sunlightDirection = Direction3d.xyZ light.azimuth light.elevation
        , shadows = True
        , dimensions = ( Pixels.pixels width, Pixels.pixels height )
        , camera = camera loadedData
        , clipDepth = Length.meters 0.1
        , background = Scene3d.backgroundColor (Color.hsl 0.57 0.48 0.75)
        , entities =
            [ loadedData.cachedBuilding
                |> Scene3d.rotateAround Axis3d.z
                    (Angle.degrees
                        (1.5 * cos (2 * pi * Duration.inSeconds timeElapsed / 7))
                    )
            ]
        }


building : MaterialInputs -> Entity coordinates
building { brick, shingles, copper, grass } =
    let
        materials =
            { brick = Material.nonmetal brick
            , shingles = Material.nonmetal shingles
            , copper = Material.nonmetal copper
            , grass = Material.nonmetal grass
            }
    in
    Scene3d.group
        [ frontBlock materials
        , rightBlock materials
        , leftBlock materials

        -- TODO: , centerBlock
        -- TODO: , backBlock
        , frontTowers materials
        , centerTower materials
        , ground materials
        ]


type alias Materials coordinates =
    { brick : Material.Uniform coordinates
    , shingles : Material.Uniform coordinates
    , copper : Material.Uniform coordinates
    , grass : Material.Uniform coordinates
    }


frontBlock : Materials coordinates -> Entity coordinates
frontBlock materials =
    let
        smallDormer =
            dormer materials
                ( Length.meters 0.25, Length.meters 1.3 )
                (Length.meters 0.25)
                (Length.meters 0.15)

        rightSmallDormers =
            List.map
                (\x -> Scene3d.translateBy (Vector3d.meters x -2.05 1.25) smallDormer)
                [ 2.35, 1.55, 1.95 ]

        smallDormers =
            Scene3d.group
                (rightSmallDormers
                    ++ List.map (Scene3d.mirrorAcross Plane3d.yz) rightSmallDormers
                )

        largeDormer =
            dormer materials
                ( Length.meters 0.8, Length.meters 1.5 )
                (Length.meters 0.15)
                (Length.meters 0.85)

        middleDormers =
            Scene3d.group
                [ Scene3d.translateBy (Vector3d.meters 0 -2.15 1.5) <|
                    largeDormer
                , Scene3d.translateBy (Vector3d.meters -0.825 -2.15 1.5) <|
                    largeDormer
                , Scene3d.translateBy (Vector3d.meters 0.825 -2.15 1.5) <|
                    largeDormer
                ]
    in
    Scene3d.group
        [ Scene3d.translateBy (Vector3d.meters 0 -1.8 0) <|
            dormer materials
                ( Length.meters 7.6, Length.meters 0.8 )
                (Length.meters 1.25)
                (Length.meters 0.45)
        , Scene3d.translateBy (Vector3d.meters 0 -2.05 0) <|
            roofedWall materials
                ( Length.meters 5, Length.meters 1.3 )
                (Length.meters 1.25)
                (Length.meters 0.75)
        , smallDormers
        , Scene3d.translateBy (Vector3d.meters 0 -2.15 0) <|
            dormer materials
                ( Length.meters 2.7, Length.meters 1.5 )
                (Length.meters 1.5)
                (Length.meters 1)
        , middleDormers
        , Scene3d.translateBy (Vector3d.meters -1.85 -2.05 0) <|
            wall materials ( Length.meters 0.15, Length.meters 0.15, Length.meters 2.2 )
        , Scene3d.translateBy (Vector3d.meters -1.85 -2.05 2.2) <|
            towerRoof materials ( Length.meters 0.2, Length.meters 0.2 )
        , Scene3d.translateBy (Vector3d.meters -1.85 -2.05 2.2) <|
            towerRoof materials ( Length.meters 0.1, Length.meters 0.65 )
        , Scene3d.translateBy (Vector3d.meters -1.185 -2.9 0) <|
            wall materials ( Length.meters 0.1, Length.meters 0.1, Length.meters 1.25 )
        , Scene3d.translateBy (Vector3d.meters 1.185 -2.9 0) <|
            wall materials ( Length.meters 0.1, Length.meters 0.1, Length.meters 1.25 )
        , Scene3d.translateBy (Vector3d.meters -0.415 -2.9 0) <|
            wall materials ( Length.meters 0.1, Length.meters 0.1, Length.meters 1.25 )
        , Scene3d.translateBy (Vector3d.meters 0.415 -2.9 0) <|
            wall materials ( Length.meters 0.1, Length.meters 0.1, Length.meters 1.25 )
        ]


rightBlock : Materials coordinates -> Entity coordinates
rightBlock materials =
    Scene3d.group
        [ Scene3d.translateBy (Vector3d.meters 4.1 0.1 0) <|
            dormer materials
                ( Length.meters 0.9, Length.meters 4.6 )
                (Length.meters 1.5)
                (Length.meters 0.45)
        , Scene3d.translateBy (Vector3d.meters 3.95 0 0) <|
            roofedWall materials
                ( Length.meters 1.2, Length.meters 1.19 )
                (Length.meters 1.5)
                (Length.meters 0.75)
        , Scene3d.translateBy (Vector3d.meters 3.95 0 0) <|
            wall materials ( Length.meters 0.2, Length.meters 0.2, Length.meters 2.5 )
        , Scene3d.translateBy (Vector3d.meters 3.95 0 2.5) <|
            towerRoof materials ( Length.meters 0.25, Length.meters 0.2 )
        , Scene3d.translateBy (Vector3d.meters 3.95 0 2.5) <|
            towerRoof materials ( Length.meters 0.1, Length.meters 0.65 )
        ]


leftBlock : Materials coordinates -> Entity coordinates
leftBlock materials =
    Scene3d.mirrorAcross Plane3d.yz (rightBlock materials)


backBlock : Materials coordinates -> Entity coordinates
backBlock materials =
    Scene3d.group
        [ Scene3d.translateBy (Vector3d.meters 0 1.8 0) <|
            roofedWall materials
                ( Length.meters 8.3, Length.meters 0.8 )
                (Length.meters 1.5)
                (Length.meters 0.95)
        , Scene3d.translateBy (Vector3d.meters 0 2 0) <|
            roofedWall materials
                ( Length.meters 2.5, Length.meters 1.1 )
                (Length.meters 1.5)
                (Length.meters 1.25)
        , Scene3d.translateBy (Vector3d.meters 0 2.7 0) <|
            wall materials ( Length.meters 1.8, Length.meters 0.5, Length.meters 1.5 )
        ]


centerBlock : Materials coordinates -> Entity coordinates
centerBlock materials =
    Scene3d.group
        [ wall materials ( Length.meters 4.2, Length.meters 3.4, Length.meters 1.5 )
        , wall materials ( Length.meters 4.6, Length.meters 1.4, Length.meters 1.5 )
        ]


centerTower : Materials coordinates -> Entity coordinates
centerTower materials =
    Scene3d.group
        [ wall materials
            ( Length.meters 2, Length.meters 2, Length.meters 3.55 )
        , Scene3d.translateBy (Vector3d.meters 0 0 3.55) <|
            tower materials
                ( Length.meters 1.99, Length.meters 1.99, Length.meters 0.7 )
        , Scene3d.translateBy (Vector3d.meters 0 0 4.25) <|
            towerRoof materials ( Length.meters 2, Length.meters 1 )
        , Scene3d.translateBy (Vector3d.meters 0 0 5) <|
            wall materials
                ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.5 )
        , Scene3d.translateBy (Vector3d.meters 0 0 5.5) <|
            wall materials
                ( Length.meters 0.35, Length.meters 0.35, Length.meters 0.35 )
        , Scene3d.translateBy (Vector3d.meters 0 0 5.8) <|
            towerRoof materials ( Length.meters 0.4, Length.meters 0.4 )
        , Scene3d.translateBy (Vector3d.meters 0 0 5.8) <|
            towerRoof materials ( Length.meters 0.1, Length.meters 1.25 )
        ]


frontTowers : Materials coordinates -> Entity coordinates
frontTowers materials =
    Scene3d.group
        [ leftFrontTower materials
        , rightFrontTower materials
        ]


leftFrontTower : Materials coordinates -> Entity coordinates
leftFrontTower materials =
    Scene3d.group
        [ Scene3d.translateBy (Vector3d.meters 2.9 -2.25 0) <|
            wall materials ( Length.meters 0.55, Length.meters 0.55, Length.meters 2.75 )
        , Scene3d.translateBy (Vector3d.meters 2.9 -2.25 2.75) <|
            towerRoof materials ( Length.meters 0.55, Length.meters 1.15 )
        , Scene3d.translateBy (Vector3d.meters 2.9 -2.25 3.5) <|
            wall materials ( Length.meters 0.2, Length.meters 0.2, Length.meters 0.25 )
        , Scene3d.translateBy (Vector3d.meters 2.9 -2.25 3.75) <|
            towerRoof materials ( Length.meters 0.25, Length.meters 0.25 )
        , Scene3d.translateBy (Vector3d.meters 2.9 -2.25 3.75) <|
            towerRoof materials ( Length.meters 0.1, Length.meters 0.65 )
        ]


rightFrontTower : Materials coordinates -> Entity coordinates
rightFrontTower materials =
    Scene3d.mirrorAcross Plane3d.yz (leftFrontTower materials)


ground : Materials coordinates -> Entity coordinates
ground { grass } =
    Scene3d.blockWithShadow grass <|
        Block3d.centeredOn
            (Frame3d.atPoint
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 0)
                    (Length.meters -0.025)
                )
            )
            ( Length.meters 9.5, Length.meters 6.5, Length.meters 0.05 )



-- PRIMITIVES


type alias Dimensions3 =
    ( Quantity.Quantity Float Meters, Quantity.Quantity Float Meters, Quantity.Quantity Float Meters )


type alias Dimensions2 =
    ( Quantity.Quantity Float Meters, Quantity.Quantity Float Meters )


wallHelper :
    Material.Uniform coordinates
    -> Dimensions3
    -> Scene3d.Entity coordinates
wallHelper material ( width, depth, height ) =
    Scene3d.blockWithShadow material <|
        Block3d.centeredOn
            (Frame3d.atPoint
                (Point3d.xyz
                    (Length.meters 0)
                    (Length.meters 0)
                    (Quantity.half height)
                )
            )
            ( width, depth, height )


wall : Materials coordinates -> Dimensions3 -> Scene3d.Entity coordinates
wall { brick } dimensions =
    wallHelper (Material.uniform brick) dimensions


slantedRoofHelper :
    Material.Uniform coordinates
    -> Material.Uniform coordinates
    -> Dimensions3
    -> Scene3d.Entity coordinates
slantedRoofHelper triangleMaterial quadMaterial ( width, depth, height ) =
    let
        halfWidth =
            Quantity.half width

        halfDepth =
            Quantity.half depth

        offset =
            Length.meters -0.15

        offsetHalfDepth =
            Quantity.plus halfDepth offset

        frontTriangle =
            Triangle3d.from
                (Point3d.xyz (Quantity.negate halfWidth) halfDepth Quantity.zero)
                (Point3d.xyz halfWidth halfDepth Quantity.zero)
                (Point3d.xyz Quantity.zero offsetHalfDepth height)

        backTriangle =
            Triangle3d.mirrorAcross Plane3d.zx frontTriangle

        rightQuad =
            [ Triangle3d.from
                (Point3d.xyz halfWidth (Quantity.negate halfDepth) Quantity.zero)
                (Point3d.xyz halfWidth halfDepth Quantity.zero)
                (Point3d.xyz Quantity.zero offsetHalfDepth height)
            , Triangle3d.from
                (Point3d.xyz Quantity.zero offsetHalfDepth height)
                (Point3d.xyz halfWidth (Quantity.negate halfDepth) Quantity.zero)
                (Point3d.xyz Quantity.zero (Quantity.negate offsetHalfDepth) height)
            ]

        leftQuad =
            List.map (Triangle3d.mirrorAcross Plane3d.yz) rightQuad

        triangleMesh =
            Mesh.facets [ frontTriangle, backTriangle ]

        quadMesh =
            Mesh.facets (leftQuad ++ rightQuad)
    in
    Scene3d.group
        [ Scene3d.meshWithShadow triangleMaterial
            triangleMesh
            (Mesh.shadow triangleMesh)
        , Scene3d.meshWithShadow quadMaterial
            quadMesh
            (Mesh.shadow quadMesh)
        ]


straightRoofHelper :
    Material.Uniform coordinates
    -> Material.Uniform coordinates
    -> Dimensions3
    -> Scene3d.Entity coordinates
straightRoofHelper triangleMaterial quadMaterial ( width, depth, height ) =
    let
        halfWidth =
            Quantity.half width

        halfDepth =
            Quantity.half depth

        frontTriangle =
            Triangle3d.from
                (Point3d.xyz
                    (Quantity.negate halfWidth)
                    halfDepth
                    Quantity.zero
                )
                (Point3d.xyz halfWidth halfDepth Quantity.zero)
                (Point3d.xyz Quantity.zero halfDepth height)

        backTriangle =
            Triangle3d.mirrorAcross Plane3d.zx frontTriangle

        rightQuad =
            [ Triangle3d.from
                (Point3d.xyz halfWidth (Quantity.negate halfDepth) Quantity.zero)
                (Point3d.xyz halfWidth halfDepth Quantity.zero)
                (Point3d.xyz Quantity.zero halfDepth height)
            , Triangle3d.from
                (Point3d.xyz Quantity.zero halfDepth height)
                (Point3d.xyz halfWidth (Quantity.negate halfDepth) Quantity.zero)
                (Point3d.xyz Quantity.zero (Quantity.negate halfDepth) height)
            ]

        leftQuad =
            List.map (Triangle3d.mirrorAcross Plane3d.yz) rightQuad

        triangleMesh =
            Mesh.facets [ frontTriangle, backTriangle ]

        quadMesh =
            Mesh.facets (leftQuad ++ rightQuad)
    in
    Scene3d.group
        [ Scene3d.meshWithShadow
            triangleMaterial
            triangleMesh
            (Mesh.shadow triangleMesh)
        , Scene3d.meshWithShadow
            quadMaterial
            quadMesh
            (Mesh.shadow quadMesh)
        ]


wallRoof : Materials coordinates -> Dimensions3 -> Scene3d.Entity coordinates
wallRoof { shingles } (( width, depth, height ) as dimensions) =
    if Quantity.greaterThan depth width then
        Scene3d.rotateAround Axis3d.z (Angle.degrees 90) <|
            slantedRoofHelper shingles shingles ( depth, width, height )

    else
        slantedRoofHelper shingles shingles dimensions


dormerRoof : Materials coordinates -> Dimensions3 -> Scene3d.Entity coordinates
dormerRoof { brick, shingles } (( width, depth, height ) as dimensions) =
    if Quantity.greaterThan depth width then
        Scene3d.rotateAround Axis3d.z (Angle.degrees 90) <|
            straightRoofHelper brick shingles ( depth, width, height )

    else
        straightRoofHelper brick shingles dimensions


towerRoof :
    Materials coordinates
    -> Dimensions2
    -> Scene3d.Entity coordinates
towerRoof { copper } ( width, height ) =
    let
        halfWidth =
            Quantity.half width

        frontTriangle =
            Triangle3d.from
                (Point3d.xyz
                    (Quantity.negate halfWidth)
                    halfWidth
                    Quantity.zero
                )
                (Point3d.xyz halfWidth halfWidth Quantity.zero)
                (Point3d.xyz Quantity.zero Quantity.zero height)

        backTriangle =
            Triangle3d.mirrorAcross Plane3d.zx frontTriangle

        leftTriangle =
            Triangle3d.rotateAround Axis3d.z (Angle.degrees -90) frontTriangle

        rightTriangle =
            Triangle3d.mirrorAcross Plane3d.yz leftTriangle

        mesh =
            Mesh.facets
                [ frontTriangle
                , backTriangle
                , leftTriangle
                , rightTriangle
                ]
    in
    Scene3d.meshWithShadow copper
        mesh
        (Mesh.shadow mesh)



-- UTILITIES


tower : Materials coordinates -> Dimensions3 -> Scene3d.Entity coordinates
tower { copper } dimensions =
    wallHelper (Material.uniform copper) dimensions


roofedWall :
    Materials coordinates
    -> Dimensions2
    -> Quantity.Quantity Float Meters
    -> Quantity.Quantity Float Meters
    -> Scene3d.Entity coordinates
roofedWall materials ( width, depth ) wallHeight roofHeight =
    Scene3d.group
        [ wall materials ( width, depth, wallHeight )
        , Scene3d.translateBy
            (Vector3d.xyz (Length.meters 0) (Length.meters 0) wallHeight)
          <|
            wallRoof materials ( width, depth, roofHeight )
        ]


dormer :
    Materials coordinates
    -> ( Quantity.Quantity Float Meters, Quantity.Quantity Float Meters )
    -> Quantity.Quantity Float Meters
    -> Quantity.Quantity Float Meters
    -> Scene3d.Entity coordinates
dormer materials ( width, depth ) wallHeight roofHeight =
    Scene3d.group
        [ wall materials ( width, depth, wallHeight )
        , Scene3d.translateBy
            (Vector3d.xyz (Length.meters 0) (Length.meters 0) wallHeight)
            (dormerRoof materials ( width, depth, roofHeight ))
        ]
