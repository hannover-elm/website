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
import Ease
import Frame3d
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Illuminance exposing (Illuminance)
import Length exposing (Length, Meters)
import Luminance
import Material.Slider as Slider
import Material.Typography as Typography
import Palette.Tango as Tango
import Pixels
import Plane3d
import Point2d
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.Entity as Entity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh
import Scene3d.Transformation as Transformation
import SketchPlane3d
import Sphere3d
import Task exposing (Task)
import Triangle2d
import Triangle3d
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)


margin : Float
margin =
    8


aspectRatio : Float
aspectRatio =
    385 / 600


animationLength : Duration
animationLength =
    Duration.seconds 5


type State
    = Loading
    | Loaded Viewport Model


type alias Model =
    { focalPointZ : Float
    , eyePointY : Float
    , eyePointZ : Float
    , verticalFieldOfView : Float
    , timeElapsed : Duration
    , materials : MaterialInputs
    , lights : LightInputs
    , cachedBuilding : Entity.Entity Xyz
    }


type Xyz
    = Xyz


type alias MaterialInputs =
    { brick : MaterialInput
    , shingles : MaterialInput
    , copper : MaterialInput
    , grass : MaterialInput
    }


type alias LightInputs =
    { key : LightInput
    , fill : LightInput
    , back : LightInput
    }


initialModel : Model
initialModel =
    let
        materials =
            { brick =
                { baseColor =
                    Color.fromHSL
                        ( 38.499064389421264, 56.97875908940121, 78.1079560233666 )
                , roughness = 1
                , metallic = 0
                }
            , shingles =
                { baseColor =
                    Color.fromHSL ( 13.483604447958669, 100, 37.60377358490566 )
                , roughness = 0.6113207547169811
                , metallic = 0
                }
            , copper =
                { baseColor =
                    Color.fromHSL
                        ( 156.65660377358503, 36.2063492063492, 72.0377358490566 )
                , roughness = 0.7622142058283398
                , metallic = 0
                }
            , grass =
                { baseColor =
                    Color.fromHSL
                        ( 98.15094339622642, 50.18867924528302, 33.9622641509434 )
                , roughness = 0.8867924528301887
                , metallic = 0
                }
            }
    in
    { focalPointZ = 3.0068258407915627
    , eyePointY = -20
    , eyePointZ = 0.7548686466307473
    , verticalFieldOfView = 20.00266445496179
    , timeElapsed = Duration.seconds 0
    , lights =
        { key =
            { chromaticity = Chromaticity.d65
            , intensity = Illuminance.lux 10000
            , azimuth = Angle.degrees 54.351620314477074
            , elevation = Angle.degrees -45.845370877967845
            }
        , fill =
            { chromaticity = Chromaticity.d65
            , intensity = Illuminance.lux 2500
            , azimuth = Angle.degrees 83.2845085894129
            , elevation = Angle.degrees -45
            }
        , back =
            { chromaticity = Chromaticity.d65
            , intensity = Illuminance.lux 5000
            , azimuth = Angle.degrees 25.827204453623803
            , elevation = Angle.degrees -45
            }
        }
    , materials = materials
    , cachedBuilding = building materials
    }


viewpoint : Model -> Viewpoint3d Meters world
viewpoint model =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.meters 0 0 model.focalPointZ
        , eyePoint =
            Point3d.meters 0
                (model.eyePointY
                    + 0.1
                    * cos (8412.213 + Duration.inSeconds model.timeElapsed)
                )
                (model.eyePointZ
                    + 0.2
                    * cos (9157.591 + Duration.inSeconds model.timeElapsed)
                )
        , upDirection = Direction3d.positiveZ
        }


camera : Model -> Camera3d Meters world
camera model =
    Camera3d.perspective
        { viewpoint = viewpoint model
        , verticalFieldOfView = Angle.degrees model.verticalFieldOfView
        , clipDepth = Length.meters 0.1
        }


main : Program () State Msg
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
    | KeyLightChanged LightInput
    | BackLightChanged LightInput
    | FillLightChanged LightInput
    | Resized Viewport


type alias MaterialInput =
    { baseColor : Color
    , roughness : Float
    , metallic : Float
    }


type alias LightInput =
    { chromaticity : Chromaticity
    , intensity : Illuminance
    , azimuth : Angle
    , elevation : Angle
    }


type alias Viewport =
    { width : Float, height : Float }


init : () -> ( State, Cmd Msg )
init () =
    ( Loading
    , Browser.Dom.getViewport
        |> Task.map (\viewport -> viewport.scene)
        |> Task.perform Resized
    )


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case ( msg, state ) of
        ( Resized viewport, Loading ) ->
            ( Loaded viewport initialModel, Cmd.none )

        ( _, Loading ) ->
            ( Loading, Cmd.none )

        ( Resized viewport, Loaded _ model ) ->
            ( Loaded viewport (updateLoaded msg model), Cmd.none )

        ( _, Loaded viewport model ) ->
            ( Loaded viewport (updateLoaded msg model), Cmd.none )


updateLoaded : Msg -> Model -> Model
updateLoaded msg model =
    case msg of
        FocalPointZChanged focalPointZ ->
            { model | focalPointZ = focalPointZ }

        EyePointYChanged eyePointY ->
            { model | eyePointY = eyePointY }

        EyePointZChanged eyePointZ ->
            { model | eyePointZ = eyePointZ }

        VerticalFieldOfViewChanged verticalFieldOfView ->
            { model | verticalFieldOfView = verticalFieldOfView }

        BrickChanged brick ->
            mapMaterials (\materials -> { materials | brick = brick }) model

        ShinglesChanged shingles ->
            mapMaterials (\materials -> { materials | shingles = shingles }) model

        CopperChanged copper ->
            mapMaterials (\materials -> { materials | copper = copper }) model

        GrassChanged grass ->
            mapMaterials (\materials -> { materials | grass = grass }) model

        KeyLightChanged key ->
            mapLights (\lights -> { lights | key = key }) model

        FillLightChanged fill ->
            mapLights (\lights -> { lights | fill = fill }) model

        BackLightChanged back ->
            mapLights (\lights -> { lights | back = back }) model

        Simulated timeSinceLastFrame ->
            { model
                | timeElapsed =
                    Quantity.plus (Duration.milliseconds timeSinceLastFrame)
                        model.timeElapsed
            }

        Resized _ ->
            model


mapMaterials : (MaterialInputs -> MaterialInputs) -> Model -> Model
mapMaterials f model =
    { model | materials = f model.materials }


mapLights : (LightInputs -> LightInputs) -> Model -> Model
mapLights f model =
    { model | lights = f model.lights }


subscriptions : State -> Sub Msg
subscriptions state =
    let
        onResized =
            Browser.Events.onResize
                (\width height ->
                    Resized { width = toFloat width, height = toFloat height }
                )

        onSimulated =
            Browser.Events.onAnimationFrameDelta Simulated
    in
    case state of
        Loading ->
            onResized

        Loaded _ _ ->
            Sub.batch [ onResized, onSimulated ]


view : State -> Html Msg
view state =
    case state of
        Loading ->
            text ""

        Loaded viewport model ->
            Html.div []
                [ scene viewport model
                , controls viewport model
                ]


controls : Viewport -> Model -> Html Msg
controls viewport model =
    Html.div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "640px"
        , style "width" "280px"
        , style "max-height" "100vh"
        , style "overflow-y" "auto"
        , style "padding-right" "20px"
        , style "display" "none"
        ]
        [ slider
            { label = "focalPointZ"
            , onChange = FocalPointZChanged
            , min = -10
            , max = 10
            , value = model.focalPointZ
            }
        , slider
            { label = "eyePointY"
            , onChange = EyePointYChanged
            , min = -20
            , max = 5
            , value = model.eyePointY
            }
        , slider
            { label = "eyePointZ"
            , onChange = EyePointZChanged
            , min = -10
            , max = 20
            , value = model.eyePointZ
            }
        , slider
            { label = "verticalFieldOfView"
            , onChange = VerticalFieldOfViewChanged
            , min = 0
            , max = 100
            , value = model.verticalFieldOfView
            }
        , slider
            { label = "timeElapsed"
            , onChange = Simulated
            , min = 0
            , max = 9999
            , value = Duration.inSeconds model.timeElapsed
            }
        , lightControl
            { label = "key"
            , onChange = KeyLightChanged
            , value = model.lights.key
            }
        , lightControl
            { label = "fill"
            , onChange = FillLightChanged
            , value = model.lights.fill
            }
        , lightControl
            { label = "back"
            , onChange = BackLightChanged
            , value = model.lights.back
            }
        , materialControl
            { label = "brick"
            , onChange = BrickChanged
            , value = model.materials.brick
            }
        , materialControl
            { label = "shingles"
            , onChange = ShinglesChanged
            , value = model.materials.shingles
            }
        , materialControl
            { label = "copper"
            , onChange = CopperChanged
            , value = model.materials.copper
            }
        , materialControl
            { label = "grass"
            , onChange = GrassChanged
            , value = model.materials.grass
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
    Html.div []
        [ Html.label [ Typography.caption ]
            [ text (label ++ ": " ++ String.fromFloat value) ]
        , Slider.slider
            (Slider.config
                |> Slider.setOnChange onChange
                |> Slider.setMin (Just min)
                |> Slider.setMax (Just max)
                |> Slider.setValue (Just value)
            )
        ]


materialControl :
    { label : String
    , onChange : MaterialInput -> msg
    , value : MaterialInput
    }
    -> Html msg
materialControl { label, onChange, value } =
    let
        ( hue, saturation, luminance ) =
            Color.toHSL value.baseColor

        { roughness, metallic } =
            value

        fromHue newHue =
            { value | baseColor = Color.fromHSL ( newHue, saturation, luminance ) }

        fromSaturation newSaturation =
            { value | baseColor = Color.fromHSL ( hue, newSaturation, luminance ) }

        fromLuminance newLuminance =
            { value | baseColor = Color.fromHSL ( hue, saturation, newLuminance ) }

        fromMetallic newMetallic =
            { value | metallic = newMetallic }

        fromRoughness newRoughness =
            { value | roughness = newRoughness }
    in
    Html.div []
        [ Html.label [ Typography.caption ] [ text (label ++ ":") ]
        , slider
            { label = "Hue"
            , onChange = onChange << fromHue
            , min = 0
            , max = 255
            , value = hue
            }
        , slider
            { label = "Saturation"
            , onChange = onChange << fromSaturation
            , min = 0
            , max = 100
            , value = saturation
            }
        , slider
            { label = "Luminance"
            , onChange = onChange << fromLuminance
            , min = 0
            , max = 100
            , value = luminance
            }
        , slider
            { label = "Roughness"
            , onChange = onChange << fromRoughness
            , min = 0
            , max = 1
            , value = roughness
            }
        , slider
            { label = "Metallic"
            , onChange = onChange << fromMetallic
            , min = 0
            , max = 1
            , value = metallic
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
        [ Html.label [ Typography.caption ] [ text (label ++ ":") ]
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
                \newElevation -> onChange { value | elevation = Angle.degrees newElevation }
            , min = -90
            , max = 90
            , value = Angle.inDegrees elevation
            }
        ]


scene : Viewport -> Model -> Html Msg
scene viewport model =
    let
        width =
            min 600 (viewport.width - 2 * margin)

        height =
            width * aspectRatio

        lights =
            let
                f { chromaticity, intensity, azimuth, elevation } =
                    { chromaticity = chromaticity
                    , intensity = intensity
                    , direction = Direction3d.xyZ azimuth elevation
                    }
            in
            { key =
                Scene3d.directionalLight Scene3d.castsShadows
                    (f model.lights.key)
            , fill =
                Scene3d.directionalLight Scene3d.doesNotCastShadows
                    (f model.lights.fill)
            , back =
                Scene3d.directionalLight Scene3d.doesNotCastShadows
                    (f model.lights.back)
            }
    in
    Scene3d.toHtml []
        { camera = camera model
        , dimensions = ( Pixels.pixels width, Pixels.pixels height )
        , environmentalLighting =
            Scene3d.softLighting
                { upDirection = Direction3d.positiveZ
                , above = ( Luminance.nits 5000, Chromaticity.d65 )
                , below = ( Quantity.zero, Chromaticity.d65 )
                }
        , lights = Scene3d.threeLights lights.key lights.fill lights.back
        , exposure = Exposure.fromMaxLuminance (Luminance.nits 5000)
        , whiteBalance = Scene3d.defaultWhiteBalance
        , background =
            Scene3d.backgroundColor (Color.fromHSL ( 206.1, 48.1, 74.7 ))
        }
        [ Entity.rotateAround Axis3d.z
            (Angle.degrees (1.5 * cos (Duration.inSeconds model.timeElapsed)))
            model.cachedBuilding
        ]


building : MaterialInputs -> Entity.Entity coordinates
building { brick, shingles, copper, grass } =
    let
        materials =
            { brick = Material.pbr brick
            , shingles = Material.pbr shingles
            , copper = Material.pbr copper
            , grass = Material.pbr grass
            }
    in
    Entity.group
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


frontBlock : Materials coordinates -> Entity.Entity coordinates
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


rightBlock : Materials coordinates -> Entity.Entity coordinates
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


leftBlock : Materials coordinates -> Entity.Entity coordinates
leftBlock materials =
    Scene3d.mirrorAcross Plane3d.yz (rightBlock materials)


backBlock : Materials coordinates -> Entity.Entity coordinates
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


centerBlock : Materials coordinates -> Entity.Entity coordinates
centerBlock materials =
    Scene3d.group
        [ wall materials ( Length.meters 4.2, Length.meters 3.4, Length.meters 1.5 )
        , wall materials ( Length.meters 4.6, Length.meters 1.4, Length.meters 1.5 )
        ]


centerTower : Materials coordinates -> Entity.Entity coordinates
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


frontTowers : Materials coordinates -> Entity.Entity coordinates
frontTowers materials =
    Scene3d.group
        [ leftFrontTower materials
        , rightFrontTower materials
        ]


leftFrontTower : Materials coordinates -> Entity.Entity coordinates
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


rightFrontTower : Materials coordinates -> Entity.Entity coordinates
rightFrontTower materials =
    Scene3d.mirrorAcross Plane3d.yz (leftFrontTower materials)


ground : Materials coordinates -> Entity.Entity coordinates
ground { grass } =
    Scene3d.block Scene3d.doesNotCastShadows grass <|
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
    Scene3d.block Scene3d.castsShadows material <|
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
    in
    Scene3d.group
        [ Scene3d.mesh triangleMaterial (Mesh.facets [ frontTriangle, backTriangle ])
        , Scene3d.mesh quadMaterial (Mesh.facets (leftQuad ++ rightQuad))
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
    in
    Scene3d.group
        [ Scene3d.mesh triangleMaterial (Mesh.facets [ frontTriangle, backTriangle ])
        , Scene3d.mesh quadMaterial (Mesh.facets (leftQuad ++ rightQuad))
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
    in
    Scene3d.mesh copper
        (Mesh.facets
            [ frontTriangle
            , backTriangle
            , leftTriangle
            , rightTriangle
            ]
        )



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
