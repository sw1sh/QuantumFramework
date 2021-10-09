Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumMeasurement"]

PackageScope["QuantumMeasurementQ"]


QuantumMeasurementQ[QuantumMeasurement[qs_ ? QuantumStateQ, _ ? orderQ]] := qs["OutputQudits"] <= qs["InputQudits"]

QuantumMeasurementQ[___] := False


qm_QuantumMeasurement["ValidQ"] := QuantumMeasurementQ[qm]


(* constructors *)

QuantumMeasurement[proba_Association, states : {_QuantumState..}] /;
    Length[proba] == Length[states] && Equal @@ Map[#["Dimensions"] &, states] && AllTrue[states, #["PureStateQ"] &] :=
QuantumMeasurement[
    QuantumState[
        QuantumState[
            Flatten @ kroneckerProduct[Map[#["Computational"]["StateVector"] &, states], Sqrt @ Values[proba]],
            QuantumTensorProduct[

                QuantumBasis[QuditBasis @ Prepend[First[states]["Dimensions"], Length[states]]],
                QuantumBasis[Keys[proba]]
            ]
        ],
        QuantumTensorProduct[
            QuantumBasis[QuditBasis[Length[states]]],
            First[states]["Basis"][{"Split", First[states]["Qudits"]}],
            QuantumBasis[Keys[proba]]
        ][{"Split", 1}]
    ](*[{"PermuteInput", Cycles[{Range[1 + First[states]["Qudits"]]}]}]*),
    {First[states]["Qudits"] + 1}
]


(* properties *)

$QuantumMeasurementProperties = {
    "State",
    "Distribution",
    "Outcomes", "Probabilities",
    "Mean", "States", "StateAssociation",
    "Entropy",
    "PostMeasurementState"
};


QuantumMeasurement["Properties"] := QuantumMeasurement["Properties"] = DeleteDuplicates @ Join[$QuantumMeasurementProperties, QuantumState["Properties"]]


QuantumMeasurement::undefprop = "QuantumMeasurement property `` is undefined for this basis";

(qm_QuantumMeasurement[prop_ ? propQ, args___]) /; QuantumMeasurementQ[qm] := With[{
    result = QuantumMeasurementProp[qm, prop, args]
    },
    (* don't cache Simulated* results *)
    If[ MatchQ[prop, name_String | {name_String, ___} /; StringStartsQ[name, "Simulated"]],
        result,
        QuantumMeasurementProp[qm, prop, args] = result
    ] /; !MatchQ[result, _QuantumMeasurementProp] || Message[QuantumMeasurement::undefprop, prop]
]

QuantumMeasurement[qm_QuantumMeasurement, args___] := QuantumMeasurement[QuantumState[qm["State"], args], qm["Target"]]


QuantumMeasurementProp[qm_, "Properties"] := DeleteDuplicates @ Join[$QuantumMeasurementProperties, qm["State"]["Properties"]]

QuantumMeasurementProp[QuantumMeasurement[qs_, _], "State"] := qs

QuantumMeasurementProp[QuantumMeasurement[_, target_], "Target"] := target

QuantumMeasurementProp[qm_, "Arity" | "Targets"] := Length @ qm["Target"]

QuantumMeasurementProp[qm_, "Eigenstate"] := QuantumPartialTrace[qm["State"][{"Split", qm["Qudits"]}], qm["OutputQudits"] + Range[qm["InputQudits"]]]

QuantumMeasurementProp[qm_, "PostMeasurementState"] := QuantumPartialTrace[
    qm["State"][{"Split", qm["Qudits"]}],
    Join[Range[qm["OutputQudits"]], qm["OutputQudits"] + Complement[Range[qm["InputQudits"]], qm["Target"]]]
]

QuantumMeasurementProp[qm_, "States"] := If[MatchQ[qm["Label"], "Computational"[_]],
    QuantumState[QuantumState[#, QuantumBasis[qm["InputDimensions"]]], QuantumBasis[qm["Input"]]] & /@
        qm["State"]["Computational"]["StateMatrix"],
    QuantumState[#, qm["Input"]] & /@ qm["State"]["StateMatrix"]
]

QuantumMeasurementProp[qm_, "ProbabilitiesList"] :=
    If[MatchQ[qm["Label"], "Computational"[_]], qm["Eigenstate"]["Computational"], qm["Eigenstate"]]["Probabilities"]

QuantumMeasurementProp[qm_, "Eigenvalues"] := qm["Eigenstate"]["BasisElementNames"]

QuantumMeasurementProp[qm_, "Outcomes"] :=
    If[MatchQ[qm["Label"], "Computational"[_]], QuditBasis[qm["InputDimensions"][[ qm["Target"] ]]]["Names"], qm["Eigenvalues"]]

QuantumMeasurementProp[qm_, "Distribution"] := CategoricalDistribution[
    qm["Outcomes"],
    qm["ProbabilitiesList"]
]

QuantumMeasurementProp[qm_, "DistributionInformation", args___] := Information[qm["Distribution"], args]

QuantumMeasurementProp[qm_, args :
    "Categories" | "Probabilities" | "ProbabilityTable" | "ProbabilityArray" |
    "ProbabilityPlot" |
    "TopProbabilities" | ("TopProbabilities" -> _Integer)] := qm["DistributionInformation", args]


QuantumMeasurementProp[qm_, "Entropy"] := Quantity[qm["DistributionInformation", "Entropy"] / Log[2], "Bits"]

QuantumMeasurementProp[qm_, "SimulatedMeasurement"] := RandomVariate[qm["Distribution"]]

QuantumMeasurementProp[qm_, {"SimulatedMeasurement", n_Integer}] := RandomVariate[qm["Distribution"], n]

QuantumMeasurementProp[qm_, "Mean"] := qm["Eigenvalues"] . qm["ProbabilitiesList"]

QuantumMeasurementProp[qm_, "StateAssociation" | "StatesAssociation"] := KeySort @ AssociationThread[qm["Outcomes"], qm["States"]]

QuantumMeasurementProp[qm_, "StateAmplitudes"] := Map[Simplify, #["Amplitudes"]] & /@ qm["StateAssociation"]

QuantumMeasurementProp[qm_, "StateProbabilities"] := Merge[Thread[qm["States"] -> qm["ProbabilityArray"]], Total]

QuantumMeasurementProp[qm_, "StateProbabilityTable"] := Dataset[qm["StateProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities"] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities" -> n_Integer] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities" -> n]]

QuantumMeasurementProp[qm_, "SimulatedStateMeasurement"] := qm["StateAssociation"][qm["SimulatedMeasurement"]]

QuantumMeasurementProp[qm_, {"SimulatedStateMeasurement", n_}] := Part[qm["StateAssociation"], Key /@ qm[{"SimulatedMeasurement", n}]]

QuantumMeasurementProp[qm_, "MeanState"] := qm["Mean"] /. qm["StateAssociation"]


(* state properties *)

QuantumMeasurementProp[qm_, prop_ ? propQ, args___] /;
    MatchQ[prop, Alternatives @@ Intersection[qm["State"]["Properties"], qm["Properties"]]] := qm["State"][prop, args]


(* equality *)

QuantumMeasurement /: (qm1_QuantumMeasurement ? QuantumMeasurementQ) == (qm2_QuantumMeasurement ? QuantumMeasurementQ) :=
    qm1["State"] == qm2["State"]


(* formatting *)

QuantumMeasurement /: MakeBoxes[qm_QuantumMeasurement ? QuantumMeasurementQ, format_] := Module[{icon},
    icon = If[
        AllTrue[qm["Probabilities"], NumericQ],
        Show[
            BarChart[qm["Probabilities"], Frame -> {{True, False}, {True, False}}, FrameTicks -> None],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}
        ],
        Graphics[{
            {GrayLevel[0.55], Rectangle[{0., 0.}, {0.87, 1.}]},
            {GrayLevel[0.8], Rectangle[{1.,0.}, {1.88, 2.}]},
            {GrayLevel[0.65], Rectangle[{2., 0.}, {2.88, 3.}]}},
            Background -> GrayLevel[1], ImageSize -> {Automatic, 29.029}, AspectRatio -> 1]
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

