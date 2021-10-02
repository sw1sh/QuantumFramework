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
    DeleteDuplicates @ Join[QuantumMeasurementOperator["Properties"], qmo["Operator"]["Properties"]]


(* getters *)

QuantumMeasurementOperatorProp[_[op_], "Operator"] := op


QuantumMeasurementOperatorProp[qmo_, "QuantumOperator"] := QuantumOperator[qmo["Operator"], qmo["CircuitOrder"]]

QuantumMeasurementOperatorProp[qmo_, "Type"] := Which[
    qmo["OutputQudits"] == qmo["InputQudits"],
    "Projection",
    qmo["OutputQudits"] > qmo["InputQudits"],
    "POVM",
    True,
    "Destructive"
]

QuantumMeasurementOperatorProp[qo_, "InputOrder"] := qo["CircuitOrder"]

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
    ordered = qmo["Ordered"],
    trace,
    traceQudits,
    tracedOperator
},
    trace = Complement[ordered["InputOrder"], qmo["Order"]];
    traceQudits = trace - Min[ordered["InputOrder"]] + 1;

    If[
        ordered["POVMQ"],

        QuantumOperator[QuantumOperator[
            ArrayReshape[#, {Times @@ Rest @ ordered["OutputDimensions"], ordered["InputDimension"]}] & /@ ordered["POVMElements"],
            ordered["Basis"]
        ],
            ordered["CircuitOrder"]
        ],

        tracedOperator = QuantumPartialTrace[ordered, If[ordered["POVMQ"], {# + ordered["OutputQudits"] - ordered["InputQudits"], #} & /@ trace, trace]];
        Simplify @ QuantumOperator[
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
                        QuantumPartialTrace[ordered["Output"], ordered["InputQuditOrder"]],
                        tracedOperator["Output"]
                    ],
                    "Input" -> QuantumTensorProduct[QuantumPartialTrace[ordered["Input"], ordered["InputQuditOrder"]], tracedOperator["Input"]]
                ]
            ][
                {"PermuteOutput", InversePermutation @ FindPermutation[Prepend[1 + Join[ordered["InputQuditOrder"], traceQudits], 1]]}
            ][
                {"PermuteInput", InversePermutation @ FindPermutation[Join[ordered["InputQuditOrder"], traceQudits]]}
            ],
            ordered["CircuitOrder"]
        ]
    ]
]


(* operator properties *)

QuantumMeasurementOperatorProp[qmo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qmo["Operator"]["Properties"], qmo["Properties"]], prop] := qmo["Operator"][args]

