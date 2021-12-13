Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumHamiltonianOperator"]

PackageScope["QuantumHamiltonianOperatorQ"]



QuantumHamiltonianOperatorQ[QuantumHamiltonianOperator[op_]] := QuantumOperatorQ[op]

QuantumHamiltonianOperatorQ[___] := False


(* constructors *)

QuantumHamiltonianOperator[arg : Except[_ ? QuantumOperatorQ]] := QuantumHamiltonianOperator[arg, \[FormalT]]

QuantumHamiltonianOperator[qho_ ? QuantumHamiltonianOperatorQ, args___] :=
    QuantumHamiltonianOperator[QuantumOperator[qho["QuantumOperator"], args]]

QuantumHamiltonianOperator[args__, spec_] :=
    Enclose @ QuantumHamiltonianOperator[ConfirmBy[QuantumOperator[args, "ParameterSpec" -> spec], QuantumOperatorQ]]

qho_QuantumHamiltonianOperator[qo_ ? QuantumFrameworkOperatorQ] :=
    QuantumHamiltonianOperator[qho["QuantumOperator"] @ qo]

qho_QuantumHamiltonianOperator[param : Except[_ ? QuantumFrameworkOperatorQ]] :=
    qho["QuantumOperator"][param]

