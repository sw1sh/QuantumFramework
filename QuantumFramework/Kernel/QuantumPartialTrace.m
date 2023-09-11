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
    outputIdx = qo["OutputOrderQuditMapping"],
    inputIdx  = qo["InputOrderQuditMapping"]
},
    QuantumOperator[
        QuantumPartialTrace[qo["State"], {Lookup[outputIdx, #[[1]]], Lookup[inputIdx, #[[2]]]} & /@ qudits],
        {
            DeleteElements[qo["OutputOrder"], qudits[[All, 1]]],
            DeleteElements[qo["InputOrder"], qudits[[All, 2]]]
        }
    ]
]

QuantumPartialTrace[qm_ ? QuantumMeasurementQ, qudits_] := QuantumMeasurement[QuantumPartialTrace[qm["State"], qudits]]

QuantumPartialTrace[qc_ ? QuantumCircuitOperatorQ, qudits : {{_Integer, _Integer} ..}] := Enclose @ Block[{
    outputIdx = qc["OutputOrderQuditMapping"],
    inputIdx  = qc["InputOrderQuditMapping"],
    out = qudits[[All, 1]], in = qudits[[All, 2]],
    min,
    outDims, inDims
},
    ConfirmAssert[ContainsAll[qc["FullOutputOrder"], out] && ContainsAll[qc["FullInputOrder"], in]];
    outDims = qc["OutputDimensions"][[Lookup[outputIdx, out]]];
    inDims = qc["InputDimensions"][[Lookup[inputIdx, in]]];
    ConfirmAssert[outDims == inDims];
    min = Min[Keys[outputIdx], Keys[inputIdx]];
    QuantumCircuitOperator[Reverse @ MapIndexed[{"Cup", #1[[2]]} -> {min - #2[[1]], #1[[1]]} &, Thread[{out, outDims}]]] /*
        qc /*
    QuantumCircuitOperator[MapIndexed[{"Cap", #1[[2]]} -> {min - #2[[1]], #1[[1]]} &, Thread[{in, inDims}]]]
]


QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {{_Integer, _Integer} ..}] :=
    Head[op][
        QuantumPartialTrace[op["QuantumOperator"], qudits]
    ]

QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {_Integer ..}] := QuantumPartialTrace[op, {#, #} & /@ qudits]

