Package["QuantumFramework`"]

PackageExport["QuantumMeasurementOperator"]

PackageScope["QuantumMeasurementOperatorQ"]



QuantumMeasurementOperatorQ[QuantumMeasurementOperator[op_]] := QuantumOperatorQ[op]

QuantumMeasurementOperatorQ[___] := False


(* constructors *)


QuantumMeasurementOperator[qmo_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[qmo["Operator"], args]]


QuantumMeasurementOperator[args : Except[_ ? QuantumOperatorQ]] :=
    Enclose @ With[{op = ConfirmBy[QuantumOperator[args], QuantumOperatorQ]}, QuantumMeasurementOperator[op, op["InputOrder"]]]

QuantumMeasurementOperator[args : PatternSequence[Except[_ ? QuantumOperatorQ], ___]] :=
    Enclose @ QuantumMeasurementOperator[ConfirmBy[QuantumOperator[args], QuantumOperatorQ]]


(* mutation *)

QuantumMeasurementOperator[op_ ? QuantumMeasurementOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[op["QuantumOperator"], args]]

QuantumMeasurementOperator[op_ ? QuantumOperatorQ, args__] :=
    QuantumMeasurementOperator[QuantumOperator[op, args]]


(* composition *)

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qs_ ? QuantumStateQ] := Enclose @ With[{
    qudits = If[qmo["POVMQ"], qmo["Arity"], 1]
},
    ConfirmAssert[qs["OutputQudits"] >= qmo["Arity"], "Not enough output qudits"];

    QuantumMeasurement[qmo["SuperOperator"][{"Ordered", 1, qs["OutputQudits"]}][qs]
        [{"Permute", FindPermutation[
            Join[
                Range[qudits],
                qudits + qs["OutputQudits"] + Range[qs["InputQudits"]],
                qudits + Range[qs["OutputQudits"]]
            ]]}]
        [{"Split", qudits + qs["InputQudits"]}],

        qmo["Order"]
    ]
]

(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumOperatorQ] := With[{
    newOp = qmo["SuperOperator"]
},
    QuantumMeasurementOperator[newOp @ op, qmo["Order"]]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[op_ ? QuantumMeasurementOperatorQ] := Module[{
    order, ordered1, ordered2
},
    (* ordering will insert identities to fit operator into specified range *)
    order = {"Ordered", Min[op["InputOrder"], qmo["InputOrder"]], Max[op["InputOrder"], qmo["InputOrder"]]};
    ordered1 = qmo["SuperOperator"][order];
    ordered2 = op["SuperOperator"][order];

    ordered1 = QuantumOperator[
        QuantumTensorProduct[
            (* put identities on the left of first operator for each unmatched output qudit of the second operator *)
            QuantumOperator[
                QuantumOperator[{"Identity", Take[ordered2["OutputDimensions"], Max[ordered2["OutputQudits"] - ordered1["InputQudits"], 0]]}],
                With[{qb = QuantumPartialTrace[
                        ordered2["Output"],
                        Complement[Range[ordered2["OutputQudits"]], Range[Max[ordered2["OutputQudits"] - ordered1["InputQudits"], 0]]]
                    ]
                },
                    QuantumBasis[qb, qb["Dual"]]
                ]
            ],
            ordered1
        ],
        ordered2["OutputOrder"]
    ];

    QuantumMeasurementOperator[
        ordered1 @ ordered2 //
            (* permute two measured qudits based on given operator orders, left-most first *)
            (#[{"PermuteOutput", PermutationCycles[Ordering @ Ordering @ DeleteDuplicates @ Join[op["Order"], qmo["Order"]]]}] &)
            ,
        Union[qmo["Order"], op["Order"]]
    ]
]


(qmo_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ)[qm_QuantumMeasurement] := (qmo @ qm["State"]["Transpose"])


(* equality *)

QuantumMeasurementOperator /: (qmo1_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) ==
    (qmo2_QuantumMeasurementOperator ? QuantumMeasurementOperatorQ) := qmo1["MatrixRepresentation"] == qmo2["MatrixRepresentation"]

