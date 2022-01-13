Package["Wolfram`QuantumFramework`"]

PackageScope[$SparseArrayBox]



$SparseArrayBox = GraphicsBox[
    RasterBox[SparseArray[Automatic, {10, 10}, 0.93, {1, {{0, 1, 1, 4, 4, 5, 6, 6, 8, 10, 13},
        {{4}, {6}, {7}, {10}, {5}, {9}, {3}, {8}, {5}, {7}, {2}, {3}, {7}}}, {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}}],
        {{0, 0}, {10, 10}}, {0, 1}
    ],
    AspectRatio -> 1,
    Axes -> False,
    Frame -> False,
    FrameLabel -> {None, None},
    FrameStyle -> Directive[
        Opacity[0.5],
        Thickness[Tiny],
        RGBColor[0.368417, 0.506779, 0.709798]
    ],
    FrameTicks -> None,
    ImageSize -> Dynamic[{Automatic, 3.5 (CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification])}]
]

