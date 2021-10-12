Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumMeasurementOperatorProp"]



$QuantumMeasurementOperatorProperties = {
    "QuantumOperator", "Target",
    "Operator", "Basis", "MatrixRepresentation", "POVMElements",
    "OrderedMatrixRepresentation", "OrderedPOVMElements",
    "Arity", "Dimensions", "Order", "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors",
    "ProjectionQ", "POVMQ",
    "SuperOperator"
};


QuantumMeasurementOperator["Properties"] := DeleteDuplicates @ Join[
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

QuantumMeasurementOperatorProp[qmo_, "Properties"] :=
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["Operator"]["Properties"]]


(* getters *)

QuantumMeasurementOperatorProp[_[op_, _], "Operator" | "QuantumOperator"] := op

QuantumMeasurementOperatorProp[_[_, target_], "Target"] := target

QuantumMeasurementOperatorProp[qmo_, "Targets"] := Length[qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, "Type"] := Which[
    qmo["OutputQudits"] == qmo["InputQudits"],
    "Projection",
    qmo["OutputQudits"] > qmo["InputQudits"],
    "POVM",
    True,
    "Destructive"
]

QuantumMeasurementOperatorProp[qmo_, "ProjectionQ"] := qmo["Type"] === "Projection"

QuantumMeasurementOperatorProp[qmo_, "POVMQ"] := qmo["Type"] === "POVM"

QuantumMeasurementOperatorProp[qmo_, "POVMElements"] := If[qmo["POVMQ"], qmo["Tensor"], qmo["Projectors"]]

QuantumMeasurementOperatorProp[qmo_, "Sort"] := QuantumMeasurementOperator[qmo["QuantumOperator"]["Sort"], qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, prop : "Ordered" | {"Ordered", __}] := QuantumMeasurementOperator[qmo["QuantumOperator"][prop], qmo["Target"]]

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
    eigenBasis, outputBasis, inputBasis, operator
},
    trace = Complement[qmo["InputOrder"], qmo["Target"]];
    traceQudits = trace - Min[qmo["InputOrder"]] + 1;

    If[
        qmo["POVMQ"],

        qmo["Operator"],

        tracedOperator = QuantumPartialTrace[
            qmo,
            If[qmo["POVMQ"], {# + qmo["OutputQudits"] - qmo["InputQudits"], #} & /@ trace, trace]
        ];

        eigenBasis = QuditBasis[
            MapIndexed[
                Interpretation[Tooltip[Style[#, Bold], StringTemplate["Eigenvalue ``"][First @ #2]], {#1, #2}] &,
                tracedOperator["Eigenvalues"]
            ],
            tracedOperator["Eigenvectors"]
        ];

        outputBasis = QuantumPartialTrace[qmo["Output"], qmo["Target"] - qmo["FirstInputQudit"] + 1 + qmo["OutputQudits"] - qmo["InputQudits"]];
        inputBasis = QuantumPartialTrace[qmo["Input"], qmo["Target"] - qmo["FirstInputQudit"] + 1];

        (* construct *)
        operator = QuantumOperator[
            Map[kroneckerProduct @@ Append[IdentityMatrix /@ qmo["InputDimensions"][[traceQudits]], #] &, tracedOperator["Projectors"]],

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
        operator = QuantumOperator[
            operator,
            QuantumBasis[
                "Output" -> QuantumTensorProduct[
                    eigenBasis,
                    outputBasis,
                    tracedOperator["Output"]
                ],
                "Input" -> QuantumTensorProduct[inputBasis, tracedOperator["Input"]]
            ]
        ];

        (* permute and set order *)
        QuantumOperator[
            operator[
                {"PermuteOutput", InversePermutation @ FindPermutation[Prepend[1 + Join[traceQudits, qmo["Target"]], 1]]}
            ][
                {"PermuteInput", InversePermutation @ FindPermutation[Join[traceQudits, qmo["Target"]]]}
            ],
            {Automatic, qmo["OutputOrder"]}
        ]
    ]
]

QuantumMeasurementOperatorProp[qmo_, "POVM"] := QuantumMeasurementOperator[qmo["SuperOperator"], qmo["Target"]]

QuantumMeasurementOperatorProp[qmo_, "Numeric"] := QuantumMeasurementOperator[qmo["Operator"]["Numeric"], qmo["Target"]]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["Operator"]["Properties"], qmo["Properties"]], prop] := qmo["Operator"][args]

