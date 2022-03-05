Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumPartialTrace"]



QuantumPartialTrace[arg_, {}] := arg

QuantumPartialTrace[qb_QuditBasis, qudits : {_Integer ..}] := qb[{"Delete", qudits}]


QuantumPartialTrace[qb_QuantumBasis, qudits : {{_Integer, _Integer} ..}] := Enclose @ Module[{},
    ConfirmAssert[Length[qudits] <= Min[qb["OutputRank"], qb["InputRank"]]];
    ConfirmAssert[qb["OutputDimensions"][[qudits[[All, 1]]]] == qb["InputDimensions"][[qudits[[All, -1]]]]];
    QuantumBasis[qb,
        "Output" -> QuantumPartialTrace[qb["Output"], qudits[[All, 1]]],
        "Input" -> QuantumPartialTrace[qb["Input"], qudits[[All, -1]]]
    ]
]

QuantumPartialTrace[qb_QuantumBasis, qudits : {_Integer ..}] := QuantumBasis[qb, "Output" -> QuantumPartialTrace[qb["Output"], qudits]]

QuantumPartialTrace[qs_QuantumState, qudits : {_Integer ..}] :=
    QuantumState[
        QuantumState[
            Simplify @ MatrixPartialTrace[qs["Computational"]["DensityMatrix"], qudits, qs["Dimensions"]],
            QuantumBasis[qs["Dimensions"][[Complement[Range[qs["Qudits"]], qudits]]]]
        ],
        QuantumBasis @@ QuantumPartialTrace[QuantumTensorProduct[qs["Output"], qs["Input"]], qudits][
            {"Split", qs["OutputQudits"] - Count[qudits, q_ /; q <= qs["OutputQudits"]]}
        ]
    ]

QuantumPartialTrace[qs_QuantumState, qudits : {{_Integer, _Integer} ..}] := With[{
    basis = QuantumPartialTrace[qs["Basis"], qudits]
},
    QuantumState[
        QuantumState[
            Flatten @ TensorContract[qs["Tensor"], MapAt[qs["Output"]["NameRank"] + # &, qudits, {All, 2}]],
            QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
        ],
        basis
    ]
]


QuantumPartialTrace[qo_QuantumOperator, qudits : {{_Integer, _Integer} ..}] :=
    QuantumOperator[
        QuantumPartialTrace[qo["State"], # - {Min[qo["OutputOrder"]], Min[qo["InputOrder"]]} + 1 & /@ qudits],
        DeleteCases[qo["InputOrder"], Alternatives @@ qudits[[All, -1]]]
    ]


QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {{_Integer, _Integer} ..}] :=
    Head[op][
        QuantumPartialTrace[op[If[QuantumCircuitOperatorQ[op], "CircuitOperator", "QuantumOperator"]], qudits](*,
        op["Order"] /. q_Integer :> q - Count[qudits[[All, -1]], _ ? (LessThan[q])]*)
    ]

QuantumPartialTrace[op_ ? QuantumFrameworkOperatorQ, qudits : {_Integer ..}] := QuantumPartialTrace[op, {#, #} & /@ qudits]

QuantumPartialTrace[qm_ ? QuantumMeasurementQ, qudits_] := QuantumMeasurement[QuantumPartialTrace[qm["State"], qudits]]

