Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumCircuitOperator"]

PackageScope["QuantumCircuitOperatorQ"]



QuantumCircuitOperatorQ[QuantumCircuitOperator[KeyValuePattern[{"Operators" -> operators_, "Label" -> _}]]] :=
    VectorQ[Unevaluated @ operators, QuantumFrameworkOperatorQ]

QuantumCircuitOperatorQ[___] := False


(* constructors *)

QuantumCircuitOperator[operators_ /; VectorQ[operators, QuantumFrameworkOperatorQ]] :=
    QuantumCircuitOperator[<|"Operators" -> operators, "Label" -> RightComposition @@ (#["Label"] & /@ operators)|>]

QuantumCircuitOperator[operators_ /; VectorQ[operators, QuantumFrameworkOperatorQ], label_] :=
    QuantumCircuitOperator[<|"Operators" -> operators, "Label" -> label|>]

QuantumCircuitOperator[op : Except[_ ? QuantumCircuitOperatorQ, _ ? QuantumFrameworkOperatorQ], args___] := QuantumCircuitOperator[{op}, args]

QuantumCircuitOperator[op_ ? QuantumCircuitOperatorQ, args__] := QuantumCircuitOperator[op["Operators"], args]

QuantumCircuitOperator[qco_ ? QuantumCircuitOperatorQ | {qco_ ? QuantumCircuitOperatorQ}] := qco


(* composition *)

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[op_ ? QuantumFrameworkOperatorQ] :=
    QuantumCircuitOperator[Prepend[qco["Operators"], op], qco["Label"][op["Label"]]]

(qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ)[qs_ ? QuantumStateQ] := Fold[ReverseApplied[Construct], qs, qco["Operators"]]

op_QuantumMeasurementOperator[qco_QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Append[qco["Operators"], op], op["Label"][qco["Label"]]]


QuantumCircuitOperator /: Composition[qcos___QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Catenate[Reverse @ #["Operators"] & /@ Reverse @ {qcos}], Composition @@ (#["Label"] & /@ {qcos})]

QuantumCircuitOperator /: RightComposition[qcos___QuantumCircuitOperator ? QuantumCircuitOperatorQ] :=
    QuantumCircuitOperator[Catenate[#["Operators"] & /@ {qcos}], RightComposition @@ Reverse @ (#["Label"] & /@ {qcos})]


(* equality *)

QuantumCircuitOperator /: Equal[qco : _QuantumCircuitOperator ... ] := Equal @@ (#["CircuitOperator"] & /@ {qco})

