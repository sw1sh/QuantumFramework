Package["QuantumFramework`"]

PackageExport["QuantumDiscreteState"]

PackageScope["QuantumDiscreteStateQ"]


QuantumDiscreteState::inState = "is invalid";

QuantumDiscreteState::inBasis = "has invalid basis";

QuantumDiscreteState::incompatible = "is incompatible with its basis";


QuantumDiscreteStateQ[QuantumDiscreteState[state_, basis_]] :=
    (stateQ[state] || (Message[QuantumDiscreteState::inState]; False)) &&
    (QuantumBasisQ[basis] || (Message[QuantumDiscreteState::inBasis]; False)) &&
    (Length[state] === basis["Dimension"] || (Message[QuantumDiscreteState::incompatible]; False))

QuantumDiscreteStateQ[___] := False


(* basis argument input *)

QuantumDiscreteState[state_ ? stateQ, basisArgs___] /; !QuantumBasisQ[basisArgs] := Enclose @ Module[{
    basis, multiplicity
},
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ];
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    QuantumDiscreteState[
        PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
        basis
    ]
]


(* association input *)

QuantumDiscreteState[state_ ? AssociationQ, basisArgs___] /; VectorQ[Values[state]] := Enclose @ Module[{
    basis = ConfirmBy[QuantumBasis[basisArgs], QuantumBasisQ], multiplicity},
    multiplicity = basisMultiplicity[Length[state], basis["Dimension"]];
    basis = ConfirmBy[QuantumBasis[basis, multiplicity], QuantumBasisQ];
    ConfirmAssert[ContainsOnly[normalBasisElementName /@ Keys[state], basis["NormalBasisElementNames"]]];
    QuantumDiscreteState[
        Values @ KeyMap[normalBasisElementName, state][[Key /@ basis["NormalBasisElementNames"]]] /. _Missing -> 0,
        basis
    ]
]


(* eigenvalues input *)

QuantumDiscreteState["Eigenvalues" -> eigenvalues_ ? VectorQ, basisArgs___] := With[{
    basis = QuantumBasis[basisArgs]
},
    QuantumDiscreteState[
        Total @ MapThread[#1 #2 &, {eigenvalues, basis["Projectors"]}],
        basis
    ] /; Length[eigenvalues] == basis["Dimension"]
]


(* expand basis *)

QuantumDiscreteState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumDiscreteState[
    state,
    QuantumTensorProduct[basis, QuantumBasis[Max[2, Length[state] - basis["Dimension"]]]]
] /; Length[state] > basis["Dimension"]


(* pad state *)

QuantumDiscreteState[state_ ? stateQ, basis_ ? QuantumBasisQ] := QuantumDiscreteState[
    PadRight[state, Table[basis["Dimension"], TensorRank[state]]],
    basis
] /; Length[state] < basis["Dimension"]


(* Mutation *)

(* change of basis *)

QuantumDiscreteState[qds_ ? QuantumDiscreteStateQ, newBasis_ ? QuantumBasisQ] /;
    qds["BasisElementDimension"] === newBasis["BasisElementDimension"] :=
Module[{
    state = qds["State"],
    matrixRepresentation = qds["Basis"]["OutputMatrix"],
    newMatrixRepresentation = newBasis["Matrix"]
},
    Switch[qds["StateType"],
    "Vector",
    QuantumDiscreteState[
        PseudoInverse[newMatrixRepresentation] . (matrixRepresentation . state),
        newBasis
    ],
    "Matrix",
    QuantumDiscreteState[
        PseudoInverse[newMatrixRepresentation] . (matrixRepresentation . state . PseudoInverse[matrixRepresentation]) . newMatrixRepresentation,
        newBasis]
   ]
  ]


(* renew basis *)

QuantumDiscreteState[qds_ ? QuantumDiscreteStateQ, args___] := With[{
    newBasis = QuantumBasis[qds["Basis"], args]},
    If[ qds["Basis"] === newBasis,
        qds,
        QuantumDiscreteState[qds["State"], newBasis]
    ]
]

