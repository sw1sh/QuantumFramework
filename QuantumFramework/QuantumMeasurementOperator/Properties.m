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
    qmo["OutputQudits"] == qmo["InputQudits"] + 1,
    "POVM",
    True,
    "Unknown"
]

QuantumMeasurementOperatorProp[qmo_, "ProjectionQ"] := qmo["Type"] === "Projection"

QuantumMeasurementOperatorProp[qmo_, "POVMQ"] := qmo["Type"] === "POVM"

QuantumMeasurementOperatorProp[qmo_, "POVMElements"] := If[qmo["POVMQ"], qmo["Tensor"], qmo["Projectors"]]

QuantumMeasurementOperatorProp[qmo_, prop : "Ordered" | {"Ordered", _}] := QuantumMeasurementOperator[qmo["QuantumOperator"][prop], qmo["Order"]]

QuantumMeasurementOperatorProp[qmo_, "OrderedPOVMElements"] := qmo[{"OrderedPOVMElements", qmo["MaxArity"]}]

QuantumMeasurementOperatorProp[qmo_, {"OrderedPOVMElements", arity_Integer}] := If[qmo["POVMQ"],
    qmo[{"OrderedTensor", arity}],
    projector /@ qmo[{"OrderedMatrix", arity}]
]

QuantumMeasurementOperatorProp[qmo_, "Operators"] := If[qmo["POVMQ"],
    AssociationThread[Range[0, Length[qmo["Tensor"]] - 1], QuantumOperator[#, QuantumBasis["Output" -> qmo["Basis"]["Input"]], qmo["Order"]] & /@ qmo["Tensor"]],
    AssociationThread[Eigenvalues[qmo["Matrix"]], QuantumOperator[projector @ #, qmo["Basis"], qmo["Order"]] & /@ Eigenvectors[qmo["OrderedMatrix"]]]
]

QuantumMeasurementOperatorProp[qmo_, "SuperOperator"] := Module[{
    traceQudits = Complement[Range[qmo["MaxArity"]], qmo["Order"]],
    ordered = qmo["Ordered"],
    tracedOperator
},
    tracedOperator = QuantumPartialTrace[ordered, traceQudits];
    If[
        tracedOperator["POVMQ"],

        QuantumOperator[
            Power[ArrayReshape[#, Table[tracedOperator["InputDimension"], 2]], 1 / 2] & /@ tracedOperator["Tensor"],
            tracedOperator["Basis"]
        ],

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
                        QuantumPartialTrace[ordered["Output"], ordered["Order"]],
                        tracedOperator["Output"]
                    ],
                    "Input" -> QuantumTensorProduct[QuantumPartialTrace[ordered["Input"], ordered["Order"]], tracedOperator["Input"]]
                ]
            ][
                {"PermuteOutput", InversePermutation @ FindPermutation[Prepend[1 + Join[qmo["Order"], traceQudits], 1]]}
            ][
                {"PermuteInput", InversePermutation @ FindPermutation[Join[qmo["Order"], traceQudits]]}
            ],
            Range[ordered["Arity"]]
        ]
    ]
]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["QuantumOperator"]["Properties"], qmo["Properties"]], prop] := qmo["QuantumOperator"][args]

