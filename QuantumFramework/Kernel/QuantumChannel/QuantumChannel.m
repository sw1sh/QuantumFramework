Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumChannel"]

PackageScope["QuantumChannelQ"]



QuantumChannelQ[QuantumChannel[qo_]] := QuantumOperatorQ[qo] &&
    qo["OutputQudits"] - qo["InputQudits"] >= 1

QuantumChannelQ[___] := False


QuantumChannel::incOp = "QuantumOperators should have identical orders"

QuantumChannel[opArgs_List, args___] := With[{ops = QuantumOperator[#, args] & /@ opArgs},
    QuantumChannel[QuantumOperator[StackQuantumOperators[ops], {Prepend[ops[[1]]["InputOrder"], 0], ops[[1]]["InputOrder"]}]]
        /; Equal @@ (#["Order"] & /@ ops) || Message[QuantumChannel::incOp]
]


(qc_QuantumChannel ? QuantumChannelQ)[qs_ ? QuantumStateQ] := QuantumPartialTrace[qc["Operator"] @ qs, Range @ qc["TraceQudits"]]

(qc_QuantumChannel ? QuantumChannelQ)[qo_ ? QuantumOperatorQ] := QuantumChannel[qc["Operator"] @ qo]

(qc1_QuantumChannel ? QuantumChannelQ)[qc2_ ? QuantumChannelQ] := Enclose @ Module[{
    top, bottom, result
},

    top = qc1["Operator"];
    bottom = qc2["Operator"];

    If[ top["FirstOutputQudit"] + qc1["TraceQudits"] <= bottom["FirstOutputQudit"] + qc2["TraceQudits"],
        bottom = QuantumOperator[bottom, {
            ReplacePart[bottom["FullOutputOrder"], Thread[List /@ Range[qc2["TraceQudits"]] -> top["FirstOutputQudit"] - Reverse @ Range[qc2["TraceQudits"]]]],
            bottom["InputOrder"]
            }
        ],
        top = QuantumOperator[top, {
            ReplacePart[top["FullOutputOrder"], Thread[List /@ Range[qc1["TraceQudits"]] -> bottom["FirstOutputQudit"] - Reverse @ Range[qc1["TraceQudits"]]]],
            top["InputOrder"]
            }
        ]
    ];
    result = top[bottom]["Sort"];
    QuantumChannel[result]
]

