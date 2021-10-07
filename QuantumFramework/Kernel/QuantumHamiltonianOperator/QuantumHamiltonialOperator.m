Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumHamiltonianOperator"]

PackageScope["QuantumHamiltonianOperatorQ"]



QuantumHamiltonianOperatorQ[QuantumHamiltonianOperator[op_, {param_, init_, final_, step_}]] := QuantumOperatorQ[op] &&
    !NumericQ[param] &&
    NumericQ[init] && NumericQ[final] &&
    With[{numericStep = step /. Automatic -> (final - init) / 100},
        NumericQ[numericStep] && (final - init) / numericStep > 0
    ]

QuantumHamiltonianOperatorQ[___] := False


(* constructors *)

QuantumHamiltonianOperator[arg_] := QuantumHamiltonianOperator[arg, \[FormalT]]

QuantumHamiltonianOperator[args___, param_Symbol] := QuantumHamiltonianOperator[args, {param}]

QuantumHamiltonianOperator[args___, {param_Symbol : \[FormalT], init_ : 0}] :=
    QuantumHamiltonianOperator[args, {param, init, init + 1}]

QuantumHamiltonianOperator[args___, {param_Symbol : \[FormalT], init_ : 0, final_ : 1}] :=
    QuantumHamiltonianOperator[args, {param, init, final, Automatic}]

QuantumHamiltonianOperator[qho_ ? QuantumHamiltonianOperatorQ, args__] :=
    QuantumHamiltonianOperator[QuantumOperator[qho["QuantumOperator"], args]]

