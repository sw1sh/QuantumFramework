Package["Wolfram`QuantumFramework`"]



$QuantumMeasurementOperatorProperties = {
    "QuantumOperator", "Target", "Targets",
    "TargetIndex",
    "Operator", "Basis", "MatrixRepresentation", "POVMElements",
    "OrderedMatrixRepresentation", "OrderedPOVMElements",
    "Arity", "Eigenqudits", "Dimensions", "Order", "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors",
    "Eigendimensions", "Eigendimension",
    "StateDimensions", "StateDimension",
    "TargetDimensions", "TargetDimension",
    "StateQudits", "TargetBasis", "StateBasis", "CanonicalBasis", "Canonical",
    "ProjectionQ", "POVMQ",
    "SuperOperator", "POVM"
};


QuantumMeasurementOperator["Properties"] := Union @ Join[
    $QuantumMeasurementOperatorProperties,
    $QuantumOperatorProperties
]

qmo_QuantumMeasurementOperator["ValidQ"] := QuantumMeasurementOperatorQ[qmo]


QuantumMeasurementOperator::undefprop = "QuantumMeasurementOperator property `` is undefined for this operator";


(qmo_QuantumMeasurementOperator[prop_ ? propQ, args___]) /; QuantumMeasurementOperatorQ[qmo] := With[{
    result = QuantumMeasurementOperatorProp[qmo, prop, args]
    },
    If[TrueQ[$QuantumFrameworkPropCache], QuantumMeasurementOperatorProp[qmo, prop, args] = result, result]
        /; !FailureQ[Unevaluated @ result] && (!MatchQ[result, _QuantumMeasurementOperatorProp] || Message[QuantumMeasurementOperator::undefprop, prop])
]

CacheProperty[QuantumMeasurementOperator][args___, value_] := PrependTo[
    DownValues[QuantumMeasurementOperatorProp],
    HoldPattern[QuantumMeasurementOperatorProp[args]] :> value
]

QuantumMeasurementOperatorProp[qmo_, "Properties"] :=
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["Operator"]["Properties"]]


(* getters *)

QuantumMeasurementOperatorProp[_[op_, _], "Operator" | "QuantumOperator"] := op

QuantumMeasurementOperatorProp[_[_, target_], "Target"] := target

QuantumMeasurementOperatorProp[qmo_, "Arity" | "Targets"] := Length[qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, "EigenIndex"] :=
    Catenate @ Position[qmo["FullOutputOrder"], _ ? NonPositive, {1}]

QuantumMeasurementOperatorProp[qmo_, "TargetIndex"] :=
    Catenate @ Lookup[PositionIndex[qmo["FullOutputOrder"]], qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, "TargetDimensions"] :=
    Part[qmo["OutputDimensions"], qmo["TargetIndex"]]

QuantumMeasurementOperatorProp[qmo_, "TargetDimension"] := Times @@ qmo["TargetDimensions"]

QuantumMeasurementOperatorProp[qmo_, "ExtraQudits"] := Count[qmo["OutputOrder"], _ ? NonPositive]

QuantumMeasurementOperatorProp[qmo_, "Eigenqudits"] := Max[qmo["ExtraQudits"], 1]

QuantumMeasurementOperatorProp[qmo_, "Eigendimensions"] :=
    qmo["OutputDimensions"][[
        If[qmo["ExtraQudits"] > 0, qmo["EigenIndex"], qmo["TargetIndex"]]
    ]]

QuantumMeasurementOperatorProp[qmo_, "Eigendimension"] := Times @@ qmo["Eigendimensions"]

QuantumMeasurementOperatorProp[qmo_, "StateQudits"] := qmo["OutputQudits"] - qmo["ExtraQudits"]

QuantumMeasurementOperatorProp[qmo_, "StateDimensions"] := Drop[qmo["Dimensions"], qmo["ExtraQudits"]]

QuantumMeasurementOperatorProp[qmo_, "StateDimension"] := Times @@ qmo["StateDimensions"]

QuantumMeasurementOperatorProp[qmo_, "TargetBasis"] := qmo["Output"]["Extract", qmo["TargetIndex"]]

QuantumMeasurementOperatorProp[qmo_, "StateBasis"] :=
    QuantumBasis[qmo["Basis"], "Output" -> Last @ qmo["Output"]["Split", qmo["ExtraQudits"]], "Input" -> qmo["Input"]]

QuantumMeasurementOperatorProp[qmo_, "CanonicalBasis"] :=
    QuantumBasis[qmo["Basis"], "Output" -> QuantumTensorProduct[qmo["TargetBasis"], qmo["StateBasis"]["Output"]], "Input" -> qmo["Input"]]


QuantumMeasurementOperatorProp[qmo_, "Canonical"] /; qmo["Eigendimension"] == qmo["TargetDimension"] := With[{
    basis = qmo["CanonicalBasis"]
},
    QuantumMeasurementOperator[
        QuantumOperator[
            QuantumState[
                qmo["State"]["State"],
                basis
            ]["PermuteOutput", InversePermutation @ FindPermutation @ qmo["Target"]],
            {Join[Range[- qmo["Targets"] + 1, 0], Drop[qmo["OutputOrder"], qmo["Eigenqudits"]]], qmo["InputOrder"]}
        ],
        Sort @ qmo["Target"]
    ]
]


QuantumMeasurementOperatorProp[qmo_, "Type"] := Which[
    Count[qmo["OutputOrder"], _ ? NonPositive] == 0 && qmo["OutputDimension"] == qmo["InputDimension"],
    "Projection",
    Count[qmo["OutputOrder"], _ ? NonPositive] > 0,
    "POVM",
    True,
    "Unknown"
]

QuantumMeasurementOperatorProp[qmo_, "ProjectionQ"] := qmo["Type"] === "Projection"

QuantumMeasurementOperatorProp[qmo_, "POVMQ"] := qmo["Type"] === "POVM"

QuantumMeasurementOperatorProp[qmo_, "POVMElements"] := If[qmo["POVMQ"], qmo["Tensor"], qmo["Projectors"]]

QuantumMeasurementOperatorProp[qmo_, "OrderedPOVMElements"] := If[qmo["POVMQ"],
    qmo["OrderedTensor"],
    projector /@ qmo["OrderedMatrix"]
]

QuantumMeasurementOperatorProp[qmo_, "Operators"] := If[qmo["POVMQ"],
    AssociationThread[Range[0, Length[qmo["Tensor"]] - 1], QuantumOperator[#, QuantumBasis["Output" -> qmo["Basis"]["Input"]], qmo["Order"]] & /@ qmo["Tensor"]],
    AssociationThread[Eigenvalues[qmo["Matrix"]], QuantumOperator[projector @ #, qmo["Basis"], qmo["Order"]] & /@ Eigenvectors[qmo["OrderedMatrix"]]]
]

QuantumMeasurementOperatorProp[qmo_, "SuperOperator"] := Module[{
    trace,
    traceQudits,
    tracedOperator,
    eigenvalues, eigenvectors, projectors,
    eigenBasis, outputBasis, inputBasis, operator
},
    trace = DeleteCases[qmo["FullInputOrder"], Alternatives @@ qmo["Target"]];
    traceQudits = trace - Min[qmo["FullInputOrder"]] + 1;
    If[
        qmo["POVMQ"],

        qmo["Operator"],

        tracedOperator = QuantumPartialTrace[
            qmo,
            If[qmo["POVMQ"], {# + qmo["OutputQudits"] - qmo["InputQudits"], #} & /@ trace, trace]
        ];

        {eigenvalues, eigenvectors} = profile["Eigensystem"] @ tracedOperator["Eigensystem", "Sort" -> True];
        projectors = tracedOperator["Projectors"];

        eigenBasis = QuditBasis[
            MapIndexed[
                Interpretation[Tooltip[Style[#, Bold], StringTemplate["Eigenvalue ``"][First @ #2]], {#1, #2}] &,
                eigenvalues
            ],
            eigenvectors
        ];

        outputBasis = QuantumPartialTrace[qmo["Output"], Catenate @ Position[qmo["FullOutputOrder"], Alternatives @@ qmo["Target"]]];
        inputBasis = QuantumPartialTrace[qmo["Input"], Catenate @ Position[qmo["FullInputOrder"], Alternatives @@ qmo["Target"]]];

        (* construct *)
        operator = QuantumOperator[
            SparseArray @ Map[kroneckerProduct[IdentityMatrix[Times @@ qmo["InputDimensions"][[traceQudits]], SparseArray], #] &, projectors],

            QuantumBasis[
                "Output" -> QuantumTensorProduct[
                    eigenBasis,
                    QuditBasis[outputBasis["Dimensions"]],
                    QuditBasis[tracedOperator["OutputDimensions"]]
                ],
                "Input" -> QuantumTensorProduct[QuditBasis[inputBasis["Dimensions"]], QuditBasis[tracedOperator["InputDimensions"]]]
            ]
        ];

        (* change back basis *)
        operator = profile["basis change"] @ QuantumOperator[
            operator,
            QuantumBasis[
                "Output" -> QuantumTensorProduct[
                    eigenBasis,
                    outputBasis,
                    tracedOperator["Output"]
                ],
                "Input" -> QuantumTensorProduct[inputBasis, tracedOperator["Input"]],
                "Label" -> qmo["Label"],
                "ParameterSpec" -> qmo["ParameterSpec"]
            ]
        ];

        (* permute and set order *)
        QuantumOperator[
            operator[
                "PermuteOutput", InversePermutation @ FindPermutation[Prepend[1 + Join[traceQudits, qmo["Target"] - Min[qmo["InputOrder"]] + 1], 1]]
            ][
                "PermuteInput", InversePermutation @ FindPermutation[Join[traceQudits, qmo["Target"] - Min[qmo["InputOrder"]] + 1]]
            ],
            {Prepend[Sort @ qmo["OutputOrder"], 0], Sort @ qmo["InputOrder"]}
        ]
    ]
]

QuantumMeasurementOperatorProp[qmo_, "POVM"] := QuantumMeasurementOperator[qmo["SuperOperator"], qmo["Target"]]

(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, prop : "Ordered" | "Sort" | "SortOutput" | "SortInput" | "Computational", args___] :=
    QuantumMeasurementOperator[qmo["QuantumOperator"][prop, args], qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___]] /;
    MemberQ[Intersection[qmo["Operator"]["Properties"], qmo["Properties"]], prop] := qmo["Operator"][args]

