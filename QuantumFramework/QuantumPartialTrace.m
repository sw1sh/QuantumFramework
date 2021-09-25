Package["QuantumFramework`"]

PackageExport["QuantumPartialTrace"]



QuantumPartialTrace[qb_QuditBasis, qudits : {_Integer...}] := Module[{
    newNames = DeleteDuplicates[#["Delete", List /@ qudits] & /@ qb["Names"]],
    replace = Replace @ Thread[Complement[Range[qb["Rank"]], qudits] -> Range[qb["Rank"] - Length[qudits]]]
},
    QuditBasis[
        newNames,
        KeyMap[MapAt[replace, 2]] @ KeySelect[! MemberQ[qudits, Last[#]] &] @ qb["BasisElements"]
    ]
]


QuantumPartialTrace[qb_QuantumBasis, qudits : {{_Integer, _Integer}..}] := Enclose @ Module[{},
    ConfirmAssert[Length[qudits] <= Min[qb["OutputRank"], qb["InputRank"]]];
    ConfirmAssert[qb["OutputDimensions"][[qudits[[All, 1]]]] == qb["InputDimensions"][[qudits[[All, -1]]]]];
    QuantumBasis[qb,
        "Output" -> QuantumPartialTrace[qb["Output"], qudits[[All, 1]]],
        "Input" -> QuantumPartialTrace[qb["Input"], qudits[[All, -1]]]
    ]
]


QuantumPartialTrace[qs_QuantumState, qudits : {{_Integer, _Integer}..}] :=
    QuantumState[Flatten @ TensorContract[qs["Tensor"], MapAt[qs["Output"]["Rank"] + # &, qudits, {All, 2}]], QuantumPartialTrace[qs["Basis"], qudits]]


QuantumPartialTrace[qo_QuantumOperator, qudits : {{_Integer, _Integer}..}] :=
    QuantumOperator[QuantumPartialTrace[qo["State"], qudits], qo["Order"]]


QuantumPartialTrace[qo_ ? QuantumFrameworkOperatorQ, qudits : {{_Integer, _Integer}..}] :=
    Head[qo][QuantumPartialTrace[qo["QuantumOperator"], qudits], qo["Order"]]

