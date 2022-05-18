Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumCircuitOperatorNames"]



$QuantumCircuitOperatorNames = {"Graph", "GroverDiffusion", "BooleanOracle"}


QuantumCircuitOperator[{"Graph", g_Graph}] := QuantumCircuitOperator[QuantumOperator["CZ", {#1, #2}] & @@@ EdgeList[IndexGraph @ g]]


QuantumCircuitOperator[{"GroverDiffusion", n_Integer ? Positive}] := QuantumCircuitOperator[{
    Splice[Table[QuantumOperator["H", {q}], {q, n}]],
    Splice[Table[QuantumOperator["X", {q}], {q, n + 1}]],
    QuantumOperator[{"Controlled", "Z", Range[n]}],
    Splice[Table[QuantumOperator["X", {q}], {q, n + 1}]],
    Splice[Table[QuantumOperator["H", {q}], {q, n}]]
}]


QuantumCircuitOperator[{"BooleanOracle", formula_}] := Enclose @ Module[{
    esop = Confirm[BooleanConvert[formula, "ESOP"]] /. And -> List,
    indices
},
    esop = Replace[esop, clause : Except[_Xor] :> {clause}]  /. Xor -> List;
    esop = Replace[esop, clause : Except[_List] :> {clause}, {1}];
	indices = PositionIndex /@ Map[If[MatchQ[#, _Not], 0, 1] &, esop, {2}];
	QuantumCircuitOperator[QuantumOperator[{"Controlled", "NOT", #[1], #[0]}] & /@ indices]
]

