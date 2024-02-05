Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumChannel"]

PackageScope["QuantumChannelQ"]



QuantumChannelQ[QuantumChannel[qo_]] := QuantumOperatorQ[qo] &&
    qo["OutputQudits"] - qo["InputQudits"] >= 0 && AllTrue[Join[qo["InputOrder"], Drop[qo["OutputOrder"], qo["OutputQudits"] - qo["InputQudits"]]], Positive]

QuantumChannelQ[___] := False


QuantumChannel[opArgs_List, args___] := Enclose @ Block[{ops = QuantumOperator[#, args]["Computational"] & /@ opArgs, order, inputDims, outputDims},
    order = Union @@@ Thread[Through[ops["Order"]]];
    inputDims = Merge[AssociationThread[#["InputOrder"], #["InputDimensions"]] & /@ ops, Identity];
    ConfirmAssert[AllTrue[inputDims, Apply[Equal]]];
    outputDims = Merge[AssociationThread[#["OutputOrder"], #["OutputDimensions"]] & /@ ops, Identity];
    ConfirmAssert[AllTrue[outputDims, Apply[Equal]]];
    inputDims = Values[inputDims][[All, 1]];
    outputDims = Values[outputDims][[All, 1]];
    QuantumChannel @ QuantumOperator[
        StackQuantumOperators[#["OrderedOutput", order[[1]], QuditBasis[outputDims]]["OrderedInput", order[[2]], QuditBasis[inputDims]["Dual"]] & /@ ops],
        MapAt[Prepend[0], order, {1}]
    ]
]

QuantumChannel[qc_ ? QuantumChannelQ, args___] := QuantumChannel[QuantumOperator[qc["QuantumOperator"], args]]

QuantumChannel[qm : _ ? QuantumMeasurementOperatorQ | _ ? QuantumMeasurementQ] := QuantumChannel[QuantumOperator[qm["POVM"]]]


(qc_QuantumChannel ? QuantumChannelQ)[opts___] := QuantumCircuitOperator[qc][opts]

(qc_QuantumChannel ? QuantumChannelQ)[qs_ ? QuantumStateQ, opts___] := QuantumPartialTrace[qc["Operator"][qs, opts], Range @ qc["TraceQudits"]]

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


(* simplify *)

Scan[
    (Symbol[#][qc_QuantumChannel, args___] ^:= qc[#, args]) &,
    {"Simplify", "FullSimplify", "Chop", "ComplexExpand"}
]


(* parameterization *)

(qc_QuantumChannel ? QuantumChannelQ)[ps : PatternSequence[p : Except[_Association], ___]] /; ! MemberQ[QuantumChannel["Properties"], p] && Length[{ps}] <= qc["ParameterArity"] :=
    qc[AssociationThread[Take[qc["Parameters"], UpTo[Length[{ps}]]], {ps}]]

(qc_QuantumChannel ? QuantumChannelQ)[rules_ ? AssociationQ] /; ContainsOnly[Keys[rules], qc["Parameters"]] :=
    QuantumChannel[qc["Operator"][rules]]

