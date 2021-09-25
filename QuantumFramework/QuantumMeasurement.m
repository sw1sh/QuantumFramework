Package["QuantumFramework`"]

PackageExport["QuantumMeasurement"]


QuantumMeasurementQ[QuantumMeasurement[ev_Association, possibleStates_]] :=
  (*VectorQ[Values[ev], NumericQ] && *)VectorQ[possibleStates, QuantumStateQ] && Length[ev] == Length[possibleStates] && Length[ev] > 0

QuantumMeasurementQ[___] := False


qb_QuantumMeasurement["ValidQ"] := QuantumMeasurementQ[qb]


$QuantumMeasurementProperties = {
    "Distribution",
    "Outcomes", "Probabilities",
    "Mean", "States", "StateAssociation",
    "Entropy",
    "PostMeasurementState"
};

QuantumMeasurement["Properties"] := $QuantumMeasurementProperties


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

QuantumMeasurementProp[_, "Properties"] := QuantumMeasurement["Properties"]

QuantumMeasurementProp[_[weights_, _], "Weights"] := weights
QuantumMeasurementProp[_[weights_, states_], "States"] := states[[Ordering[Keys[weights]]]]

QuantumMeasurementProp[qm_, "Eigenvalues"] := Keys @ qm["Weights"]

QuantumMeasurementProp[qm_, "Distribution"] := CategoricalDistribution[qm["Weights"]]

QuantumMeasurementProp[qm_, "DistributionInformation", args___] := Information[qm["Distribution"], args]

QuantumMeasurementProp[qm_, args :
    "Categories" | "Probabilities" | "ProbabilityTable" | "ProbabilityArray" |
    "ProbabilityPlot" |
    "TopProbabilities" | ("TopProbabilities" -> _Integer) |
    "Entropy"] := qm["DistributionInformation", args]

QuantumMeasurementProp[qm_, "Outcomes"] := qm["DistributionInformation", "Categories"]

QuantumMeasurementProp[qm_, "ProbabilitiesList"] := Normal @ qm["ProbabilityArray"]

QuantumMeasurementProp[qm_, "SimulatedMeasurement"] := RandomVariate[qm["Distribution"]]

QuantumMeasurementProp[qm_, {"SimulatedMeasurement", n_Integer}] := RandomVariate[qm["Distribution"], n]

QuantumMeasurementProp[qm_, "Mean"] := qm["Eigenvalues"] . qm["ProbabilitiesList"]

QuantumMeasurementProp[qm_, "StateAssociation"] := AssociationThread[qm["Outcomes"], qm["States"]]

QuantumMeasurementProp[qm_, "StateProbabilities"] := Association @ Thread[qm["States"] -> qm["ProbabilityArray"]]

QuantumMeasurementProp[qm_, "StateProbabilityTable"] := Dataset[qm["StateProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities"] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities"]]

QuantumMeasurementProp[qm_, "TopStateProbabilities" -> n_Integer] := KeyMap[qm["StateAssociation"], Association @ qm["TopProbabilities" -> n]]

QuantumMeasurementProp[qm_, "SimulatedStateMeasurement"] := qm["StateAssociation"][qm["SimulatedMeasurement"]]

QuantumMeasurementProp[qm_, {"SimulatedStateMeasurement", n_}] := Part[qm["StateAssociation"], Key /@ qm[{"SimulatedMeasurement", n}]]

QuantumMeasurementProp[qm_, "MeanState"] := qm["Mean"] /. qm["StateAssociation"]

QuantumMeasurementProp[qm_, "PostMeasurementState"] := QuantumState[
    Mean[MapThread[Times, {qm["Eigenvalues"], #["State"] & /@ qm["States"]}]],
    First[qm["States"]]["Basis"]
]

QuantumMeasurement /: MakeBoxes[qm_QuantumMeasurement ? QuantumMeasurementQ, format_] := Module[{icon},
    icon = (*# /. Line[{a_, Offset[b_, c_]}] :> Sequence[] & @*)
        Show[
            BarChart[qm["Probabilities"], Frame -> {{True, False}, {True, False}}, FrameTicks -> None],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}
        ];
    BoxForm`ArrangeSummaryBox["QuantumMeasurement", qm,
        icon,
        {
            {
                BoxForm`SummaryItem[{"Measurement Outcomes: ", Length[qm["Probabilities"]]}]
            }, {
                BoxForm`SummaryItem[{"Entropy: ", Enclose[N @ ConfirmQuiet[qm["Entropy"]], $Failed &]}]
            }
        },
        {{}},
        format,
        "Interpretable" -> Automatic
    ]
]

