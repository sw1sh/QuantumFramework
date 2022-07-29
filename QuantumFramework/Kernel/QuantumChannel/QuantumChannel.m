Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumChannel"]

PackageScope["QuantumChannelQ"]



QuantumChannelQ[QuantumChannel[qo_]] := QuantumOperatorQ[qo] &&
    qo["OutputQudits"] - qo["InputQudits"] >= 0

QuantumChannelQ[___] := False


QuantumChannel[opArgs_List, args___] := Enclose @ Block[{ops = QuantumOperator[#, args]["Computational"] & /@ opArgs, order, inputDims, outputDims},
    order = Range @@@ (MinMax /@ Thread[#["Order"] & /@ ops]);
    inputDims = Merge[AssociationThread[#["InputOrder"], #["InputDimension"]] & /@ ops, Identity];
    ConfirmAssert[AllTrue[inputDims, Apply[Equal]]];
    outputDims = Merge[AssociationThread[#["OutputOrder"], #["OutputDimension"]] & /@ ops, Identity];
    ConfirmAssert[AllTrue[outputDims, Apply[Equal]]];
    inputDims = Values[inputDims][[All, 1]];
    outputDims = Values[outputDims][[All, 1]];
    QuantumChannel[
        StackQuantumOperators[#["OrderedOutput", order[[1]], QuditBasis[outputDims]]["OrderedInput", order[[2]], QuditBasis[inputDims]] & /@ ops]
    ]
]


(qc_QuantumChannel ? QuantumChannelQ)[qs_ ? QuantumStateQ] := QuantumPartialTrace[qc["Operator"] @ qs, Range @ qc["TraceQudits"]]

(qc_QuantumChannel ? QuantumChannelQ)[qo_ ? QuantumOperatorQ] := QuantumChannel[qc["Operator"] @ qo]

(qc1_QuantumChannel ? QuantumChannelQ)[qc2_ ? QuantumChannelQ] := Enclose @ Module[{
    top, bottom, traceQudits, result
},
    top = qc1["SortOutput"];
    bottom = qc2["SortOutput"];

    traceQudits = qc1["TraceQudits"] + qc2["TraceQudits"];
    top = QuantumOperator[top,
        {Join[1 - Drop[Reverse[Range[traceQudits]], qc2["TraceQudits"]], Drop[top["OutputOrder"], qc1["TraceQudits"]]], top["InputOrder"]}
    ];
    bottom = QuantumOperator[bottom,
        {Join[1 - Take[Reverse[Range[traceQudits]], qc2["TraceQudits"]], Drop[bottom["OutputOrder"], qc2["TraceQudits"]]], bottom["InputOrder"]}
    ];
    result = top[bottom]["SortOutput"];
    QuantumChannel[result]
]


(* equality *)

QuantumChannel /: Equal[qc : _QuantumChannel ... ] := Equal @@ (#["QuantumOperator"] & /@ {qc})
