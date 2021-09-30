Package["QuantumFramework`"]

PackageScope["QuantumMeasurementOperatorProp"]



$QuantumMeasurementOperatorProperties = {
    "QuantumOperator",
    "Operator", "Basis", "MatrixRepresentation", "POVMElements",
    "OrderedMatrixRepresentation", "OrderedPOVMElements",
    "Arity", "Dimensions", "Order", "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors",
    "ProjectionQ", "POVMQ",
    "SuperOperator"
};

$QuantumMeasurementOperatorProperties = DeleteDuplicates @ Join[
    $QuantumMeasurementOperatorProperties,
    $QuantumOperatorProperties
]

QuantumMeasurementOperator["Properties"] := $QuantumMeasurementOperatorProperties

qmo_QuantumMeasurementOperator["ValidQ"] := QuantumMeasurementOperatorQ[qmo]


QuantumMeasurementOperator::undefprop = "QuantumMeasurementOperator property `` is undefined for this operator";


(qmo_QuantumMeasurementOperator[prop_ ? propQ, args___]) /; QuantumMeasurementOperatorQ[qmo] := With[{
    result = QuantumMeasurementOperatorProp[qmo, prop, args]
    },
    (QuantumMeasurementOperatorProp[qmo, prop, args] = result)
        /; !FailureQ[Unevaluated @ result] && (!MatchQ[result, _QuantumMeasurementOperatorProp] || Message[QuantumMeasurementOperator::undefprop, prop])
]

QuantumMeasurementOperatorProp[qmo_, "Properties"] :=
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["QuantumOperator"]["Properties"]]


(* getters *)

QuantumMeasurementOperatorProp[_[op_], "QuantumOperator"] := op


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

QuantumMeasurementOperatorProp[qmo_, prop : "Ordered" | {"Ordered", __}] := QuantumMeasurementOperator[qmo["QuantumOperator"][prop], qmo["Order"]]

QuantumMeasurementOperatorProp[qmo_, "OrderedPOVMElements"] := qmo[{"OrderedPOVMElements", qmo["MaxArity"]}]

QuantumMeasurementOperatorProp[qmo_, {"OrderedPOVMElements", args___}] := If[qmo["POVMQ"],
    qmo[{"OrderedTensor", args}],
    projector /@ qmo[{"OrderedMatrix", args}]
]

QuantumMeasurementOperatorProp[qmo_, "Operators"] := If[qmo["POVMQ"],
    AssociationThread[Range[0, Length[qmo["Tensor"]] - 1], QuantumOperator[#, QuantumBasis["Output" -> qmo["Basis"]["Input"]], qmo["Order"]] & /@ qmo["Tensor"]],
    AssociationThread[Eigenvalues[qmo["Matrix"]], QuantumOperator[projector @ #, qmo["Basis"], qmo["Order"]] & /@ Eigenvectors[qmo["OrderedMatrix"]]]
]

QuantumMeasurementOperatorProp[qmo_, "SuperOperator"] := Module[{
    trace = Complement[qmo["InputOrder"], qmo["Order"]],
    traceQudits,
    ordered = qmo["Ordered"],
    tracedOperator
},
    traceQudits = trace - Min[qmo["Order"]] + 1;

    If[
        qmo["POVMQ"],

        QuantumOperator[QuantumOperator[
            ArrayReshape[#, {Times @@ Rest @ qmo["OutputDimensions"], qmo["InputDimension"]}] & /@ qmo["POVMElements"],
            qmo["Basis"]
        ],
            qmo["Order"]
        ],

        tracedOperator = QuantumPartialTrace[ordered, If[qmo["POVMQ"], {# + qmo["OutputQudits"] - qmo["InputQudits"], #} & /@ trace, trace]];
        QuantumOperator[
            QuantumOperator[
                Map[kroneckerProduct @@ Prepend[IdentityMatrix /@ ordered["InputDimensions"][[traceQudits]], #] &, tracedOperator["Projectors"]],

                QuantumBasis[
                    "Output" -> QuantumTensorProduct[
                        QuditBasis[
                            MapIndexed[
                                Interpretation[Tooltip[Style[#, Bold], StringTemplate["Eigenvalue ``"][First @ #2]], {#1, #2}] &,
                                tracedOperator["Eigenvalues"]
                            ],
                            tracedOperator["Eigenvectors"]
                        ],
                        QuantumPartialTrace[ordered["Output"], ordered["QuditOrder"]],
                        tracedOperator["Output"]
                    ],
                    "Input" -> QuantumTensorProduct[QuantumPartialTrace[ordered["Input"], ordered["QuditOrder"]], tracedOperator["Input"]]
                ]
            ][
                {"PermuteOutput", InversePermutation @ FindPermutation[Prepend[1 + Join[qmo["QuditOrder"], traceQudits], 1]]}
            ][
                {"PermuteInput", InversePermutation @ FindPermutation[Join[qmo["QuditOrder"], traceQudits]]}
            ],
            qmo["Order"]
        ]
    ]
]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["QuantumOperator"]["Properties"], qmo["Properties"]], prop] := qmo["QuantumOperator"][args]

