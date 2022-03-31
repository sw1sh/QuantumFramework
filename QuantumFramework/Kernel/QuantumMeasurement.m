Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurement"]

PackageScope["QuantumMeasurementQ"]


QuantumMeasurementQ[QuantumMeasurement[qmo_ ? QuantumMeasurementOperatorQ]] := True

QuantumMeasurementQ[___] := False


qm_QuantumMeasurement["ValidQ"] := QuantumMeasurementQ[qm]


(* constructors *)

QuantumMeasurement[qs_ ? QuantumStateQ, target_ ? targetQ] := QuantumMeasurement[QuantumMeasurementOperator[qs["Operator"], target]]

QuantumMeasurement[qo_ ? QuantumFrameworkOperatorQ, target_ ? targetQ] := QuantumMeasurement[QuantumMeasurementOperator[qo, target]]

QuantumMeasurement[proba_Association, states : {_ ? QuantumStateQ..}] /;
    Length[proba] == Length[states] && Equal @@ Map[#["Dimensions"] &, states] :=
QuantumMeasurement[
    QuantumState[
        QuantumState[
            ArrayReshape[
                Transpose[
                    TensorProduct[
                        Sqrt @ Values[proba],
                        MapThread[Times, {Sqrt @ Values[proba], #["Computational"]["Normalized"]["DensityTensor"] & /@ states}]
                    ],
                    Cycles[{RotateRight @ Reverse @ Range[Length[states] + 1]}]
                ],
                Table[Length[states] First[states]["Dimension"], 2]
            ],
            QuantumTensorProduct[
                QuantumBasis[Keys[proba]],
                QuantumBasis[First[states]["Dimensions"]]
            ]
        ],
        QuantumTensorProduct[
            QuantumBasis[Keys[proba]],
            First[states]["Basis"]
        ][{"Split", 1}]
    ],
    Range[First[states]["Qudits"]]
]


(* properties *)

$QuantumMeasurementProperties = {
    "QuantumOperator",
    "Distribution",
    "Outcomes", "Probabilities",
    "Mean", "States", "StateAssociation",
    "Entropy",
    "PostMeasurementState"
};


QuantumMeasurement["Properties"] := QuantumMeasurement["Properties"] =
    DeleteDuplicates @ Join[$QuantumMeasurementProperties, QuantumMeasurementOperator["Properties"]]


QuantumMeasurement::undefprop = "QuantumMeasurement property `` is undefined for this basis";

(qm_QuantumMeasurement[prop_ ? propQ, args___]) /; QuantumMeasurementQ[qm] := With[{
    result = QuantumMeasurementProp[qm, prop, args]
    },
    (* don't cache Simulated* results *)
    If[ ! TrueQ[$QuantumFrameworkPropCache] || MatchQ[prop, name_String | {name_String, ___} /; StringStartsQ[name, "Simulated"]],
        result,
        QuantumMeasurementProp[qm, prop, args] = result
    ] /; !MatchQ[result, _QuantumMeasurementProp] || Message[QuantumMeasurement::undefprop, prop]
]

QuantumMeasurement[qm_QuantumMeasurement, args___] := QuantumMeasurement[QuantumMeasurementOperator[qm["QuantumOperator"], args]]


QuantumMeasurementProp[qm_, "Properties"] := DeleteDuplicates @ Join[$QuantumMeasurementProperties, qm["QuantumOperator"]["Properties"]]

QuantumMeasurementProp[QuantumMeasurement[qmo_], "QuantumOperator"] := qmo

QuantumMeasurementProp[qm_, "Operator"] :=
    QuantumOperator[qm["QuantumOperator"]["Operator"], {Join[qm["OutputOrder"], qm["InputOrder"]], qm["InputOrder"]}]

QuantumMeasurementProp[qm_, "TargetBasis"] := qm["Output"][{"Extract", qm["Eigenqudits"] + qm["TargetIndex"]}]

QuantumMeasurementProp[qm_, "StateBasis"] :=
    QuantumBasis["Output" -> Last @ qm["Output"][{"Split", qm["Eigenqudits"]}], "Input" -> qm["Input"]]

QuantumMeasurementProp[qm_, "CanonicalBasis"] :=
    QuantumBasis[QuantumTensorProduct[qm["TargetBasis"], qm["StateBasis"]["Output"]], qm["Input"]]

QuantumMeasurementProp[qm_, "Canonical"] := QuantumMeasurement @ QuantumMeasurementOperator[
    QuantumOperator[qm["State"], {Range[- qm["Targets"] + 1, 0], qm["InputOrder"]}, qm["CanonicalBasis"]],
    qm["Target"]
]


QuantumMeasurementProp[qm_, "Computational"] := QuantumMeasurement[
    qm["Operator"]["Computational"],
    qm["Target"]
]


QuantumMeasurementProp[qm_, "StateDual"] := qm["State"][{"Split", qm["Qudits"]}][{"PermuteOutput",
    FindPermutation @ Catenate[{#1, #3, #2}] & @@
        TakeList[Range[qm["Qudits"]], {qm["Eigenqudits"], qm["StateQudits"], qm["InputQudits"]}]
}][{"Split", qm["Eigenqudits"] + qm["InputQudits"]}]


QuantumMeasurementProp[qm_, "Arity" | "Targets"] := Length @ qm["Target"]

QuantumMeasurementProp[qm_, "StateQudits"] := qm["OutputQudits"] - qm["Eigenqudits"]

QuantumMeasurementProp[qm_, "StateDimensions"] := Drop[qm["Dimensions"], qm["Eigenqudits"]]

QuantumMeasurementProp[qm_, "StateDimension"] := Times @@ qm["StateDimensions"]

QuantumMeasurementProp[qm_, "EigenDimensions"] := Take[qm["OutputDimensions"], qm["Eigenqudits"]]

QuantumMeasurementProp[qm_, "EigenDimension"] := Times @@ qm["EigenDimensions"]

QuantumMeasurementProp[qm_, "Eigenstate"] :=
    QuantumPartialTrace[qm["State"], qm["Eigenqudits"] + Range[qm["StateQudits"]]]

QuantumMeasurementProp[qm_, "PureQ"] := TrueQ[qm["InputQudits"] == 0]

QuantumMeasurementProp[qm_, "MixedQ"] := ! qm["PureQ"]

QuantumMeasurementProp[qm_, "Eigenqudits"] := Length @ qm["OutputOrder"]

QuantumMeasurementProp[qm_, "PostMeasurementState"] := QuantumPartialTrace[
    qm["State"],
    Join[Range[qm["Eigenqudits"]], qm["Eigenqudits"] + Complement[Range[qm["StateQudits"]], qm["Target"]]]
]

QuantumMeasurementProp[qm_, "MixedStates"] := With[{rep = If[qm["PureStateQ"], 1, 2]},
    Which[
        MatchQ[qm["LabelHead"], "Eigen"],
        QuantumState[ArrayReshape[#, Table[qm["StateDimension"], rep]], qm["StateBasis"]] & /@
            qm["StateDual"]["StateMatrix"],
        MatchQ[qm["LabelHead"], "Computational"],
        QuantumState[QuantumState[ArrayReshape[#, Table[qm["StateDimension"], rep]], QuantumBasis[qm["InputDimensions"]]], qm["StateBasis"]] & /@
            qm["Computational"]["StateDual"]["StateMatrix"],
        True,
        QuantumState[ArrayReshape[#, Table[qm["StateDimension"], rep]], qm["StateBasis"]] & /@
            qm["Canonical"]["StateDual"]["StateMatrix"]
    ]
]

QuantumMeasurementProp[qm_, "States"] := If[qm["PureStateQ"], qm["MixedStates"], Plus @@@ Partition[qm["MixedStates"], qm["EigenDimension"] qm["InputDimension"]]]

QuantumMeasurementProp[qm_, "ProbabilitiesList"] :=
    If[MatchQ[qm["LabelHead"], "Computational"], qm["Eigenstate"]["Computational"], qm["Eigenstate"]]["Probabilities"]

QuantumMeasurementProp[qm_, "Eigenvalues"] := qm["Eigenstate"]["Names"]

QuantumMeasurementProp[qm_, "Outcomes"] := Which[
    MatchQ[qm["LabelHead"], "Eigen"],
    qm["Eigenvalues"],
    MatchQ[qm["LabelHead"], "Computational"],
    QuditBasis[qm["StateDimensions"][[ qm["TargetIndex"] ]]]["Names"],
    True,
    qm["Canonical"]["State"][{"Split", qm["Targets"]}]["Output"]["Names"]
]

QuantumMeasurementProp[qm_, "MixedOutcomes"] := If[
    qm["PureStateQ"],
    qm["Outcomes"],
    QuantumTensorProduct @@@ Tuples[{qm["Outcomes"], #["Dual"] & /@ qm["Outcomes"]}]
]

QuantumMeasurementProp[qm_, "Distribution"] := CategoricalDistribution[
    qm["Outcomes"],
    Normal @ Chop @ N @ qm["ProbabilitiesList"]
]

QuantumMeasurementProp[qm_, "Probabilities"] := AssociationThread[
    qm["Outcomes"],
    Normal @ qm["ProbabilitiesList"]
]

QuantumMeasurementProp[qm_, "DistributionInformation", args___] := Information[qm["Distribution"], args]

QuantumMeasurementProp[qm_, args :
    "Categories" | "Probabilities" | "ProbabilityTable" | "ProbabilityArray" |
    "ProbabilityPlot" |
    "TopProbabilities" | ("TopProbabilities" -> _Integer)] := qm["DistributionInformation", args]


QuantumMeasurementProp[qm_, "Entropy"] := TimeConstrained[Quantity[qm["DistributionInformation", "Entropy"] / Log[2], "Bits"], 1]

QuantumMeasurementProp[qm_, "SimulatedMeasurement"] := RandomVariate[qm["Distribution"]]

QuantumMeasurementProp[qm_, {"SimulatedMeasurement", n_Integer}] := RandomVariate[qm["Distribution"], n]

QuantumMeasurementProp[qm_, "Mean"] := qm["Eigenvalues"] . qm["ProbabilitiesList"]

QuantumMeasurementProp[qm_, "StateAssociation" | "StatesAssociation"] := Part[
    KeySort @ AssociationThread[qm["Outcomes"], qm["States"]],
    Catenate @ SparseArray[qm["ProbabilitiesList"]]["ExplicitPositions"]
]

QuantumMeasurementProp[qm_, "StateAmplitudes"] := Map[Simplify, #["Amplitudes"]] & /@ qm["StateAssociation"]

QuantumMeasurementProp[qm_, "StateProbabilities"] := Select[Chop /@ Merge[Thread[qm["States"] -> qm["ProbabilitiesList"]], Total], # != 0 &]

QuantumMeasurementProp[qm_, "StateProbabilityTable"] := Dataset[qm["StateProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities"] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities" -> n_Integer] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities" -> n]]

QuantumMeasurementProp[qm_, "SimulatedStateMeasurement"] := qm["StateAssociation"][qm["SimulatedMeasurement"]]

QuantumMeasurementProp[qm_, {"SimulatedStateMeasurement", n_}] := Part[qm["StateAssociation"], Key /@ qm[{"SimulatedMeasurement", n}]]

QuantumMeasurementProp[qm_, "MeanState"] := qm["Mean"] /. qm["StateAssociation"]


(* qmo properties *)

QuantumMeasurementProp[qm_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[qm["QuantumOperator"]["Properties"], qm["Properties"]]] := qm["QuantumOperator"][prop, args]


(* equality *)

QuantumMeasurement /: Equal[qms : _QuantumMeasurement ...] :=
    Equal @@ (#["State"] & /@ {qms})


(* formatting *)

QuantumMeasurement /: MakeBoxes[qm_QuantumMeasurement ? QuantumMeasurementQ, format_] := Module[{icon},
    icon = With[{proba = TimeConstrained[qm["Probabilities"], 1]},
        If[
            ! FailureQ[proba] && AllTrue[proba, NumericQ],
            Show[
                BarChart[Chop /@ N @ proba, Frame -> {{True, False}, {True, False}}, FrameTicks -> None],
                ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}
            ],
            Graphics[{
                {GrayLevel[0.55], Rectangle[{0., 0.}, {0.87, 1.}]},
                {GrayLevel[0.8], Rectangle[{1.,0.}, {1.88, 2.}]},
                {GrayLevel[0.65], Rectangle[{2., 0.}, {2.88, 3.}]}},
                Background -> GrayLevel[1], ImageSize -> {Automatic, 29.029}, AspectRatio -> 1]
        ]
    ];
    BoxForm`ArrangeSummaryBox["QuantumMeasurement", qm,
        icon,
        {
            {
                BoxForm`SummaryItem[{"Target: ", qm["Target"]}]
            },
            {
                BoxForm`SummaryItem[{"Measurement Outcomes: ", Length[qm["Probabilities"]]}]
            }
        },
        {
            {
                BoxForm`SummaryItem[{"Entropy: ", Enclose[N @ ConfirmQuiet[qm["Entropy"]], $Failed &]}]
            }
        },
        format,
        "Interpretable" -> Automatic
    ]
]

