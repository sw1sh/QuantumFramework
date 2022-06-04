Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPartialTrace"]



QuantumPartialTrace[arg_, {}] := arg

QuantumPartialTrace[qb_QuditBasis, qudits : {_Integer ..}] := qb["Delete", qudits]


QuantumPartialTrace[qb_QuantumBasis, qudits : {{_Integer, _Integer} ..}] := Enclose @ Module[{},
    ConfirmAssert[Length[qudits] <= Min[qb["OutputRank"], qb["InputRank"]]];
    ConfirmAssert[qb["OutputDimensions"][[qudits[[All, 1]]]] == qb["InputDimensions"][[qudits[[All, -1]]]]];
    QuantumBasis[qb,
        "Output" -> QuantumPartialTrace[qb["Output"], qudits[[All, 1]]],
        "Input" -> QuantumPartialTrace[qb["Input"], qudits[[All, -1]]]
    ]
]

QuantumPartialTrace[qb_QuantumBasis, qudits : {_Integer ..}] := QuantumBasis[qb, "Output" -> QuantumPartialTrace[qb["Output"], qudits]]

QuantumPartialTrace[qs_QuantumState, outQudits : {_Integer ...}, inQudits : {_Integer ...}] :=
    QuantumPartialTrace[qs, Join[outQudits, qs["OutputQudits"] + inQudits]]

QuantumPartialTrace[qs_QuantumState, {}] := qs

QuantumPartialTrace[qs_QuantumState, qudits : {_Integer ..}] :=
    QuantumState[
        QuantumState[
            If[ qs["VectorQ"] && False, (* Need an efficient check for separability to be able to discard qudits this way *)
                SparseArrayFlatten @ Extract[qs["Computational"]["StateTensor"], ReplacePart[Table[All, qs["Qudits"]], Thread[qudits -> 1]]],
                MatrixPartialTrace[qs["Computational"]["DensityMatrix"], qudits, qs["Dimensions"]]
            ],
            QuantumBasis[qs["Dimensions"][[Complement[Range[qs["Qudits"]], qudits]]]]
        ],
        QuantumBasis["Output" -> #1, "Input" -> #2] & @@ QuantumPartialTrace[QuantumTensorProduct[qs["Output"], qs["Input"]], qudits][
            "Split", qs["OutputQudits"] - Count[qudits, q_ /; q <= qs["OutputQudits"]]
        ]
    ]

QuantumPartialTrace[qs_QuantumState, qudits : {{_Integer, _Integer} ..}] := With[{
    basis = QuantumPartialTrace[qs["Basis"], qudits]
},
    QuantumState[
        QuantumState[
            SparseArrayFlatten @ TensorContract[qs["Tensor"], MapAt[qs["OutputQudits"] + # &, qudits, {All, 2}]],
            QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
        ],
        basis
    ]
]

QuantumPartialTrace[qs_QuantumState] := QuantumPartialTrace[qs, Range @ qs["Qudits"]]


QuantumPartialTrace[qo_QuantumOperator, qudits : {{_Integer, _Integer} ..}] := With[{
    outputIdx = AssociationThread[qo["OutputOrder"], Range[qo["OutputQudits"]]],
    inputIdx  = AssociationThread[qo["InputOrder"], Range[qo["InputQudits"]]]
},
    QuantumOperator[
        QuantumPartialTrace[qo["State"], {outputIdx[#[[1]]], inputIdx[#[[2]]]} & /@ qudits],
        {
            DeleteCases[qo["FullOutputOrder"], Alternatives @@ qudits[[All, 1]]],
            DeleteCases[qo["FullInputOrder"], Alternatives @@ qudits[[All, 2]]]
        }
    ]
]


QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {{_Integer, _Integer} ..}] :=
    Head[op][
        QuantumPartialTrace[op["QuantumOperator"], qudits]
    ]

QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {_Integer ..}] := QuantumPartialTrace[op, {#, #} & /@ qudits]

QuantumPartialTrace[qm_ ? QuantumMeasurementQ, qudits_] := QuantumMeasurement[QuantumPartialTrace[qm["State"], qudits]]

