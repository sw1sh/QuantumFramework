Package["QuantumFramework`"]

PackageScope["QuantumOperatorProp"]



$QuantumOperatorProperties = {
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "Order", "TotalOrder",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors", "Projectors",
    "ConjugateTranspose"
};

QuantumOperator["Properties"] := DeleteDuplicates @ Join[$QuantumOperatorProperties, Complement[QuantumState["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]];

QuantumOperatorProp[qo_, "Properties"] := DeleteDuplicates @ Join[QuantumOperator["Properties"], Complement[qo["State"]["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]]


qo_QuantumOperator["ValidQ"] := QuantumOperatorQ[qo]


QuantumOperator::undefprop = "QuantumOperator property `` is undefined for this state";


(* basic getters *)

QuantumOperatorProp[QuantumOperator[state_, _], "State"] := state

QuantumOperatorProp[QuantumOperator[_, order_], "Order"] := order


(qo_QuantumOperator[prop_ ? propQ, args___]) /; QuantumOperatorQ[qo] := With[{
    result = QuantumOperatorProp[qo, prop, args]
    },
    (QuantumOperatorProp[qo, prop, args] = result) /; !FailureQ[Unevaluated @ result] &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["Order"]

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["InputQudits"], Max[qo["Order"]]]

QuantumOperatorProp[qo_, "TotalOrder"] := Join[qo["Order"], Complement[Range[qo["MaxArity"]], qo["Order"]]]


QuantumOperatorProp[qo_, "TensorRepresentation" | "Tensor"] := Switch[
    qo["StateType"],
    "Vector",
    ArrayReshape[qo["StateVector"], qo["Dimensions"]],
    "Matrix",
    $Failed
]

QuantumOperatorProp[qo_, "MatrixRepresentation" | "Matrix"] := Switch[
    qo["StateType"],
    "Vector",
    ArrayReshape[qo["StateVector"], qo["MatrixNameDimensions"]],
    "Matrix",
    $Failed
]

QuantumOperatorProp[qo_, "Operator"] := Association @ Thread[qo["BasisElementNames"] -> Flatten[qo["MatrixRepresentation"]]]


QuantumOperatorProp[qo_, "OrderedMatrixRepresentation" | "OrderedMatrix"] := qo[{"OrderedMatrix", qo["MaxArity"]}]

QuantumOperatorProp[qo_, "OrderedTensorRepresentation" | "OrderedTensor"] := qo[{"OrderedTensor", qo["MaxArity"]}]

QuantumOperatorProp[qo_, {"OrderedMatrixRepresentation" | "OrderedMatrix", arity_Integer}] :=
    With[{ordered = qo[{"Ordered", arity}]}, ArrayReshape[ordered["Tensor"], ordered["MatrixNameDimensions"]]]

QuantumOperatorProp[qo_, {"OrderedTensorRepresentation" | "OrderedTensor", arity_Integer}] :=
    qo[{"Ordered", arity}]["Tensor"]


QuantumOperatorProp[qo_, {"PermuteInput", perm_Cycles}] := QuantumOperator[
    QuantumState[
        Flatten @ Transpose[
            qo["Tensor"],
            FindPermutation[Join[PermutationList[perm, qo["InputQudits"]], qo["OutputQudits"] + PermutationList[perm, qo["InputQudits"]]]]
        ],
        QuantumBasis[qo["Basis"], "Input" -> qo["Input"][{"Permute", perm}]]
    ],
    qo["Order"]
]

QuantumOperatorProp[qo_, "Ordered"] := qo[{"Ordered", qo["MaxArity"]}]

QuantumOperatorProp[qo_, {"Ordered", qudits_Integer}] := If[qo["InputDimension"] <= 1, qo,
    With[{arity = Max[qo["MaxArity"], qudits]},
        If[ arity > qo["InputQudits"],
            QuantumOperator[
                QuantumTensorProduct[
                    qo,
                    Sequence @@ Table[QuantumOperator[{"Identity", Last[qo["InputDimensions"]]}], arity - qo["InputQudits"]]
                ],
                Range[arity]
            ],
            qo
        ][{
            "PermuteInput", InversePermutation @ FindPermutation[Join[qo["Order"], Complement[Range[arity], qo["Order"]]]]
        }]
    ]
]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["MatrixRepresentation"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["MatrixRepresentation"]]


QuantumOperatorProp[qo_, "Eigenvalues"] := Eigenvalues[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvectors"] := Eigenvectors[qo["Matrix"]]

QuantumOperatorProp[qo_, "Projectors"] := projector /@ qo["Eigenvectors"]

QuantumOperatorProp[qo_, "ConjugateTranspose"] := QuantumOperator[ConjugateTranspose[qo["Matrix"]], qo["Order"]]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

