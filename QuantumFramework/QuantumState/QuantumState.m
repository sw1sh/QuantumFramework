Package["QuantumFramework`"]

PackageExport["QuantumState"]

PackageScope["QuantumStateQ"]


QuantumState::inState = "is invalid";

QuantumState::inBasis = "has invalid basis";

QuantumState::incompatible = "is incompatible with its basis";


QuantumStateQ[QuantumState[state_, basis_]] :=
    (stateQ[state] || (Message[QuantumState::inState]; False)) &&
    (QuantumBasisQ[basis] || (Message[QuantumState::inBasis]; False)) &&
    (Length[state] === basis["Dimension"] || (Message[QuantumState::incompatible]; False))

QuantumStateQ[___] := False


(* basis argument input *)

QuantumState[state_ ? stateQ, basisArgs___] /; !QuantumBasisQ[basisArgs] := Enclose @ Module[{
    basis, multiplicity
},
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ];
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    QuantumState[
        PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
        basis
    ]
]


(* association input *)

QuantumState[state_ ? AssociationQ, basisArgs___] /; VectorQ[Values[state]] := Enclose @ Module[{
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ], multiplicity},
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    ConfirmAssert[ContainsOnly[normalBasisElementName /@ Keys[state], basis["NormalBasisElementNames"]]];
    QuantumState[
        Values @ KeyMap[normalBasisElementName, state][[Key /@ basis["NormalBasisElementNames"]]] /. _Missing -> 0,
        basis
    ]
]


(* eigenvalues input *)

QuantumState["Eigenvalues" -> eigenvalues_ ? VectorQ, basisArgs___] := With[{
    basis = QuantumBasis[basisArgs]
},
    QuantumState[
        Total @ MapThread[#1 #2 &, {eigenvalues, basis["Projectors"]}],
        basis
    ] /; Length[eigenvalues] == basis["Dimension"]
]


(* expand basis *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    state,
    QuantumTensorProduct[basis, QuantumBasis[Max[2, Length[state] - basis["Dimension"]]]]
] /; Length[state] > basis["Dimension"]


(* pad state *)

QuantumState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumState[
    PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
    basis
] /; Length[state] < basis["Dimension"]


(* Mutation *)

(* change of basis *)

QuantumState[qs_ ? QuantumStateQ, newBasis_ ? QuantumBasisQ] /;
    qs["BasisElementDimension"] === newBasis["BasisElementDimension"] :=
Module[{
    state = qs["State"],
    matrixRepresentation = qs["Basis"]["Matrix"],
    newMatrixRepresentation = newBasis["Matrix"]
},
    Switch[qs["StateType"],
    "Vector",
    QuantumState[
        newMatrixRepresentation . (PseudoInverse[matrixRepresentation] . state),
        newBasis
    ],
    "Matrix",
    QuantumState[
        newMatrixRepresentation . (PseudoInverse[matrixRepresentation] . state . matrixRepresentation) . PseudoInverse[newMatrixRepresentation],
        newBasis]
    ]
]


(* renew basis *)

QuantumState[qs_ ? QuantumStateQ, args___] := With[{
    newBasis = QuantumBasis[qs["Basis"], args]},
    If[ qs["Basis"] === newBasis,
        qs,
        QuantumState[qs["State"], newBasis]
    ]
]


(* equality *)

QuantumState /: (qs1_QuantumState ? QuantumStateQ) == (qs2_QuantumState ? QuantumStateQ) :=
    qs1["Picture"] == qs2["Picture"] && qs1["Matrix"] == qs2["Matrix"]


(* composition *)

(qs1_QuantumState ? QuantumStateQ)[(qs2_QuantumState ? QuantumStateQ)] /; qs1["InputDimension"] == qs2["OutputDimension"] :=
    QuantumState[Flatten[qs1["StateMatrix"] . qs2["StateMatrix"]], QuantumBasis[<|"Input" -> qs2["Input"], "Output" -> qs1["Output"], "Label" -> qs1["Label"] @* qs2["Label"]|>]]
