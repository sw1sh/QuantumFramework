Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)


QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args]]


QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ]]


(* mutation *)

QuantumMeasurementOperator[op_ ? QuantumMeasurementOperatorQ, qb_ ? QuantumBasisQ] :=
    QuantumMeasurementOperator[QuantumOperator[op["QuantumOperator"], qb]]

QuantumMeasurementOperator[op_ ? QuantumFrameworkOperatorQ, order_ ? orderQ] := QuantumMeasurementOperator[Head[op][op, order]]

(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ Module[{ordered},
    ConfirmAssert[qs["OutputQudits"] >= qmo["Arity"], "Not enough output qudits"];

    ordered = qmo[{"Ordered", qs["OutputQudits"]}];
    ConfirmAssert[ordered["InputDimensions"][[ordered["Order"]]] == qs["OutputDimensions"][[ordered["Order"]]],
        "Operator and state dimensions don't match"
    ];
(*    (
        With[{
            stateTraceQudits = DeleteCases[Range[qs["OutputQudits"]], #]
        },
            ordered["SuperOperator"][QuantumPartialTrace[qs, stateTraceQudits]]
        ] & /@ qmo["Order"]
    )*)
    (*With[{
        operatorTraceQudits = Complement[Range[ordered["MaxArity"]], qmo["Order"]],
        stateTraceQudits = Complement[Range[qs["OutputQudits"]], qmo["Order"]]
    },
        QuantumMeasurement @
            QuantumPartialTrace[
                QuantumPartialTrace[ordered, operatorTraceQudits]["SuperOperator"][QuantumPartialTrace[qs, stateTraceQudits]],
                1 + Range[qmo["Arity"]]
            ]
    ]*)
    With[{
        stateTraceQudits = Complement[Range[qs["OutputQudits"]], qmo["Order"]]
    },
        QuantumMeasurement @
            ordered["SuperOperator"][qs]
    ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumFrameworkOperatorQ] := With[{
    newOp = qmo["QuantumOperator"][op["QuantumOperator"]]
},
    QuantumMeasurementOperator[newOp, If[QuantumMeasurementOperatorQ[op], Union[qmo["Order"], op["Order"]], qmo["Order"]]]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := qmo[qm["State"]]


(* equality *)

QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) ==
    (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) := qmo1["Matrix"] == qmo2["Matrix"]

