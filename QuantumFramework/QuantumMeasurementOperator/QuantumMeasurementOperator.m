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

QuantumMeasurementOperator[op_ ? QuantumFrameworkOperatorQ, order_ ? orderQ] := QuantumMeasurementOperator[Head[op][op, order]]

(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose[
    ConfirmAssert[qs["OutputQudits"] >= qmo["Arity"], "Not enough output qudits"];
    ConfirmAssert[qmo["InputDimensions"][[qmo["Order"]]] == qs["OutputDimensions"][[qmo["Order"]]], "Operator and state dimensions don't match"];
    QuantumTensorProduct @@ (
        With[{
            operatorTraceQudits = DeleteCases[Range[Max[qmo["MaxArity"], qs["OutputQudits"]]], #],
            stateTraceQudits = DeleteCases[Range[qs["OutputQudits"]], #]
        },
            QuantumMeasurement @
                QuantumPartialTrace[
                    QuantumPartialTrace[qmo[{"Ordered", qs["OutputQudits"]}], operatorTraceQudits]["SuperOperator"][QuantumPartialTrace[qs, stateTraceQudits]],
                    {2}
                ]
        ] & /@ qmo["Order"]
    )
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

