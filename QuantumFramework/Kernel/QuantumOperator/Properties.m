Package["Wolfram`QuantumFramework`"]

PackageScope["QuantumOperatorProp"]



$QuantumOperatorProperties = {
    "MatrixRepresentation", "Matrix",
    "TensorRepresentation", "Tensor",
    "Ordered", "Sort",
    "OrderedMatrixRepresentation", "OrderedMatrix",
    "OrderedTensorRepresentation", "OrderedTensor",
    "Arity", "MaxArity", "FullArity", "Range", "Order", "FullOrder", "InputOrder", "InputQuditOrder", "OutputOrder", "OutputQuditOrder",
    "FirstQudit", "LastQudit", "OrderQuditMapping",
    "HermitianQ", "UnitaryQ", "Eigenvalues", "Eigenvectors", "Projectors",
    "ConjugateTranspose",
    "QuantumOperator", "Operator",
    "Computational",
    "Dagger", "Dual"
};

QuantumOperator["Properties"] := DeleteDuplicates @ Join[$QuantumOperatorProperties, Complement[QuantumState["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]];

QuantumOperatorProp[qo_, "Properties"] := DeleteDuplicates @ Join[QuantumOperator["Properties"], Complement[qo["State"]["Properties"], {
    "BlochCartesianCoordinates", "BlochSphericalCoordinates", "BlochPlot"
}]]


qo_QuantumOperator["ValidQ"] := QuantumOperatorQ[qo]


QuantumOperator::undefprop = "property `` is undefined for this state";

QuantumOperator::failprop = "property `` have failed with ``";


(* basic getters *)

QuantumOperatorProp[QuantumOperator[state_, _], "State"] := state

QuantumOperatorProp[QuantumOperator[_, order_], "Order"] := order


(qo_QuantumOperator[prop_ ? propQ, args___]) /; QuantumOperatorQ[qo] := With[{
    result = QuantumOperatorProp[qo, prop, args]
    },
    (QuantumOperatorProp[qo, prop, args] = result) /; (!FailureQ[Unevaluated @ result] || Message[QuantumOperator::failprop, prop, result]) &&
        (!MatchQ[result, _QuantumOperatorProp] || Message[QuantumOperator::undefprop, prop])
]


(* computed properties *)

QuantumOperatorProp[qo_, "Arity"] := Length @ qo["Order"]

QuantumOperatorProp[qo_, "Range"] := Max[qo["Order"]] - Min[qo["Order"]] + 1

QuantumOperatorProp[qo_, "OrderQudits"] := If[qo["HasInputQ"], qo["InputQudits"], qo["OutputQudits"]]

QuantumOperatorProp[qo_, "MaxArity"] := Max[qo["OrderQudits"], qo["Arity"]]

QuantumOperatorProp[qo_, "FullArity"] := Max[qo["OrderQudits"], qo["Range"]]

QuantumOperatorProp[qo_, "FullOrder"] := Take[
    Join[Complement[Range[Max[qo["Order"]] - qo["OrderQudits"] + 1, Max[qo["Order"]]], qo["Order"]], qo["Order"]],
    - qo["OrderQudits"]
]

QuantumOperatorProp[qo_, "InputOrder"] := If[qo["HasInputQ"], Sort @ qo["FullOrder"], {}]

QuantumOperatorProp[qo_, "FirstQudit"] := Min @ qo["FullOrder"]

QuantumOperatorProp[qo_, "LastQudit"] := Max @ qo["FullOrder"]

QuantumOperatorProp[qo_, "InputQuditOrder"] := qo["Order"] - Min[qo["InputOrder"]] + 1

QuantumOperatorProp[qo_, "OutputOrder"] := If[qo["HasInputQ"],
    (* align to the right *)
    Join[Min[qo["InputOrder"]] - Reverse @ Range[qo["OutputQudits"] - qo["InputQudits"]], qo["InputOrder"]],
    Sort @ qo["FullOrder"]
]

QuantumOperatorProp[qo_, "OutputQuditOrder"] := qo["OutputOrder"] - Min[qo["OutputOrder"]] + 1

QuantumOperatorProp[qo_, "OrderQuditMapping"] := Thread[qo["FullOrder"] -> Range[qo["OrderQudits"]]]


QuantumOperatorProp[qo_, "Tensor"] := qo["StateTensor"]

QuantumOperatorProp[qo_, "TensorRepresentation"] := qo["State"]["TensorRepresentation"]


QuantumOperatorProp[qo_, "Matrix"] := qo["StateMatrix"]

QuantumOperatorProp[qo_, "MatrixRepresentation"] := qo["Computational"]["Matrix"]

QuantumOperatorProp[qo_, "Operator"] := qo["Amplitudes"]

QuantumOperatorProp[qo_, "QuantumOperator"] := qo

QuantumOperatorProp[qo_, name : "Computational" | "SchmidtBasis" | "SpectralBasis"] := QuantumOperator[qo["State"][name], qo["Order"]]


QuantumOperatorProp[qo_, prop : "OrderedMatrixRepresentation" | "OrderedMatrix"] := qo[{prop, qo["FullArity"]}]

QuantumOperatorProp[qo_, prop : "OrderedTensorRepresentation" | "OrderedTensor"] := qo[{prop, qo["FullArity"]}]

QuantumOperatorProp[qo_, {"OrderedMatrix", arity_Integer}] :=
    qo[{"Ordered", arity}]["Matrix"]

QuantumOperatorProp[qo_, {"OrderedMatrixRepresentation", arity_Integer}] :=
    qo[{"Ordered", arity}]["MatrixRepresentation"]

QuantumOperatorProp[qo_, {"OrderedTensor", arity_Integer}] :=
    qo[{"Ordered", arity}]["Tensor"]

QuantumOperatorProp[qo_, {"OrderedTensorRepresentation", arity_Integer}] :=
    qo[{"Ordered", arity}]["TensorRepresentation"]


QuantumOperatorProp[qo_, {"PermuteInput", perm_Cycles}] := QuantumOperator[
    qo["State"][{"PermuteInput", perm}],
    (*Select[Permute[qo["FullOrder"], perm], MemberQ[qo["Order"], #] &]*)
    qo["Order"]
]

QuantumOperatorProp[qo_, {"PermuteOutput", perm_Cycles}] := QuantumOperator[
    qo["State"][{"PermuteOutput", perm}],
    qo["Order"]
]

QuantumOperatorProp[qo_, {"Permute", perm_Cycles}] := With[{shift = Max[qo["OutputQudits"] - qo["InputQudits"], 0]},
qo[
    {"PermuteOutput", InversePermutation @ FindPermutation[Join[Range[shift], shift + PermutationList[perm, qo["InputQudits"]]]]}
][
    {"PermuteInput", perm}
]
]

QuantumOperatorProp[qo_, "Sort"] := QuantumOperator[qo[{
    "Permute",
    InversePermutation @ FindPermutation[qo["FullOrder"]]
}],
    qo["InputOrder"]
]


QuantumOperatorProp[qo_, "Ordered"] := qo[{"Ordered", Splice @ MinMax[qo[If[qo["InputDimension"] <= 1, "OutputOrder", "InputOrder"]]]}]

QuantumOperatorProp[qo_, {"Ordered", arity_Integer}] := qo[{"Ordered", Max[qo["Order"]] - arity + 1, Max[qo["Order"]]}]

QuantumOperatorProp[qo_, {"Ordered", from_Integer, to_Integer}] := qo[{"Ordered", Range[from, to]}]

QuantumOperatorProp[qo_, {"Ordered", order_ ? orderQ}] := With[{
    dimensions = qo[If[qo["HasInputQ"], "InputDimensions", "OutputDimensions"]]
},
    If[ Length[order] <= Length[dimensions],
        qo[{"Ordered", order,
            QuditBasis @ Extract[
                dimensions,
                List /@ (order - qo["FirstQudit"] + 1)
            ]}
        ],
        qo
    ]
]

QuantumOperatorProp[qo_, {"Ordered", from_Integer, to_Integer, qb_ ? QuditBasisQ}] := qo[{"Ordered", Range[from, to], qb}]

QuantumOperatorProp[qo_, {"Ordered", {}, ___}] := qo

QuantumOperatorProp[qo_, {"Ordered", order_ ? orderQ, qb_ ? QuditBasisQ}] := Enclose @ With[{
    arity = Length[order]
},
    ConfirmAssert[ContainsAll[order, qo["Order"]], "Given order should contain all operator order qudits"];
    ConfirmAssert[arity <= qb["Qudits"], "Order size should be less than or equal to number of qudits"];
    QuantumOperator[If[ arity > qo["OrderQudits"],
        QuantumOperator[
            QuantumTensorProduct[
                qo,
                QuantumOperator[{"Identity", qb[{"Delete", Catenate @ Position[order, Alternatives @@ qo["FullOrder"]]}]}]
            ],
            order
        ],
        qo
    ][{
        If[qo["HasInputQ"], "Permute", "PermuteOutput"],
        FindPermutation[Join[qo["FullOrder"], DeleteCases[order, Alternatives @@ qo["FullOrder"]]], order]
    }],
    order
    ]
]


QuantumOperatorProp[qo_, "HermitianQ"] := HermitianMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "UnitaryQ"] := UnitaryMatrixQ[qo["Matrix"]]

QuantumOperatorProp[qo_, "Eigenvalues"] := Eigenvalues[qo["MatrixRepresentation"]]

QuantumOperatorProp[qo_, "Eigenvectors"] := eigenvectors[qo["MatrixRepresentation"], "Sort" -> False]

QuantumOperatorProp[qo_, "Projectors"] := projector /@ qo["Eigenvectors"]

QuantumOperatorProp[qo_, "Dagger" | "ConjugateTranspose"] := QuantumOperator[
    ConjugateTranspose[qo["Matrix"]], qo["Basis"]["Dagger"], Take[qo["Order"], UpTo[qo["OutputQudits"]]]]

QuantumOperatorProp[qo_, "Dual"] := QuantumOperator[Conjugate[qo["Matrix"]], qo["Basis"]["Dual"], qo["Order"]]


(* state properties *)

QuantumOperatorProp[qo_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[qo["State"]["Properties"], qo["Properties"]], prop] := qo["State"][args]

