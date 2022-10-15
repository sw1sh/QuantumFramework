Package["Wolfram`QuantumFramework`"]

PackageExport[FromQuESTLink]
PackageExport[ToQuESTLink]

PackageScope[QuESTCompile]
PackageScope[QuESTApply]

PackageImport["QuEST`"]



FromQuESTLink[name_Symbol] /; Context[name] == "QuEST`Gate`" := ToUpperCase @ SymbolName[name]

FromQuESTLink[Subscript[Depol, order__][args___]] := QuantumChannel[{"Depol", args}, {order} + 1]

FromQuESTLink[Subscript[C, order__Integer][op_]] := QuantumOperator[{"C", FromQuESTLink[op], {order} + 1}]

FromQuESTLink[QuEST`Gate`R[angle_, ops_]] := QuantumOperator[{"R", angle, Splice[FromQuESTLink /@ Flatten[{Replace[ops, t_Times :> List @@ t]}]]}]

FromQuESTLink[Subscript[M, order__Integer]] := QuantumMeasurementOperator[{order} + 1]

FromQuESTLink[Subscript[name_Symbol, order__Integer]] := QuantumOperator[FromQuESTLink[name], {order} + 1]
FromQuESTLink[Subscript[name_Symbol, order__Integer][args___]] := QuantumOperator[{FromQuESTLink[name], args}, {order} + 1]

FromQuESTLink[expr_] := Missing["NotImplemented", expr]

FromQuESTLink[ops_List] := Enclose @ With[{width = Max[Cases[ops, Subscript[_, order__Integer] :> {order}, All]] + 1}, QuantumCircuitOperator[Confirm @ FromQuESTLink[#] & /@ ops]]



ToQuESTLink[qo_QuantumOperator] /; MatchQ[qo["Dimensions"], {2 ..}] := With[{order = qo["TargetOrder"] - 1},
	Enclose[Confirm @ ToQuESTLink[qo["Label"], order], Subscript[U, Sequence @@ order][qo["MatrixRepresentation"]] &]
]
ToQuESTLink[qco_QuantumCircuitOperator] := Enclose @ Flatten[Confirm @* ToQuESTLink /@ qco["Operators"]]

ToQuESTLink[label_, order_List] := Replace[label, {
	name : "H" | "S" | "T" | "X" | "Y" | "Z" | "SWAP" :> Subscript[Symbol["QuEST`Gate`" <> name], Sequence @@ order],
	name : "RX" | "RY" | "RZ" :> Subscript[Symbol["QuEST`Gate`" <> Capitalize[ToLowerCase[name]]], Sequence @@ order],
	"I" :> Subscript[QuEST`Gate`Id, Sequence @@ order],
	"NOT" :> Subscript[QuEST`Gate`X, Sequence @@ order],
	Subscript["C", subLabel_][control1_, control0_] :> Join[{Subscript[C, Sequence @@ (control1 - 1)][ToQuESTLink[subLabel, order]]}, Subscript[X, # - 1] & /@ control0],
	Subscript["R", subLabel_CircleTimes][angle_] /; Length[subLabel] == Length[order] :> QuEST`Gate`R[angle, Times @@ MapThread[ToQuESTLink[#1, {#2}] &, {List @@ subLabel, order}]],
	Subscript["R", subLabel_][angle_] :> QuEST`Gate`R[angle, ToQuESTLink[subLabel, order]],
	"P"[phase_] :> Subscript[QuEST`Gate`Ph, Sequence @@ order][phase],
	"PhaseShift"[shift_] :> Subscript[QuEST`Gate`Ph, Sequence @@ order][2 Pi / 2 ^ shift],
	HoldForm[phase_] :> Subscript[QuEST`Gate`G, Sequence @@ order][phase],
	_ :> Missing["NotImplemented", label]
 }
]

ToQuESTLink[arg_] :=  Failure["Unknown", <|"MessageTemplate" -> "Unknown QuEST operator: ``", "MessageParameters" -> arg|>]


QuESTCompile[qco_QuantumCircuitOperator] := Enclose @ QuantumOperator[CalcCircuitMatrix[Confirm @ ToQuESTLink[qco]]]["Reverse"]

QuESTApply[qco_QuantumCircuitOperator, qs_QuantumState] := Enclose @ Block[{s = CreateQureg[qs["OutputQudits"]], c = Confirm @ ToQuESTLink[qco]},
    InitStateFromAmps[s, Sequence @@ Transpose[ReIm @ qs["Reverse"]["StateVector"]]];
    ApplyCircuit[s, c];
    QuantumState[Flatten[GetQuregMatrix[s]]]["Reverse"]
]

