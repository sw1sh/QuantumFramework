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
        MatrixPartialTrace[qs["DensityMatrix"], qudits, qs["Dimensions"]],
        QuantumBasis["Output" -> #1, "Input" -> #2] & @@ QuantumPartialTrace[qs["QuditBasis"], qudits][
            "Split", qs["OutputQudits"] - Count[qudits, q_ /; q <= qs["OutputQudits"]]
        ]
    ]

QuantumPartialTrace[qs_QuantumState, qudits : {{_Integer, _Integer} ..}] := With[{
    basis = QuantumPartialTrace[qs["Basis"], qudits]
},
    QuantumState[
        SparseArrayFlatten @ TensorContract[qs["StateTensor"], MapAt[qs["OutputQudits"] + # &, qudits, {All, 2}]],
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

QuantumPartialTrace[qo_QuantumOperator] := QuantumPartialTrace[qo, Intersection @@ qo["Order"]]


QuantumPartialTrace[qm_ ? QuantumMeasurementQ, qudits_] := QuantumMeasurement[QuantumPartialTrace[qm["State"], qudits]]

QuantumPartialTrace[qm_ ? QuantumMeasurementQ] := QuantumPartialTrace[qm, Range[qm["Eigenqudits"]]]


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

QuantumPartialTrace[qc_QuantumCircuitOperator] := QuantumPartialTrace[qc, Intersection @@ qc["Order"]]


QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {{_Integer, _Integer} ..}] :=
    Head[op][
        QuantumPartialTrace[op["QuantumOperator"], qudits]
    ]

QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {_Integer ..}] := QuantumPartialTrace[op, {#, #} & /@ qudits]

