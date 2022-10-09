Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumChannelNames"]



$QuantumChannelNames = {
    "BitFlip", "PhaseFlip", "BitPhaseFlip", "Depolarizing",
    "AmplitudeDamping", "GeneralizedAmplitudeDamping",
    "PhaseDamping"
}

quantumChannel[qo_ ? QuantumOperatorQ] := QuantumChannel @
    QuantumOperator[
        qo,
        {Prepend[qo["InputOrder"], 0], qo["InputOrder"]},
        "Output" -> QuditBasis @ KeyMap[Replace[{qn_QuditName, 1} :> {QuditName[Subscript["K", qn["Name"]]], 1}]] @
            qo["Output"]["Representations"]
    ]

quantumChannel[ops_List, order_ ? orderQ, basisArgs___] := With[{
    qb = QuantumTensorProduct[
        QuantumBasis[Length[order] ^ Length[ops]],
        QuantumBasis[QuantumBasis[QuantumBasis[basisArgs]["Output"], QuantumBasis[basisArgs]["Output"]], Length[order]]
    ]
},
    quantumChannel[QuantumOperator[QuantumState[Flatten[kroneckerProduct @@@ Tuples[arg, Length[order]]], qb], order]]
]

quantumChannel[args___] := quantumChannel[QuantumOperator[args]]


QuantumChannel[{"AmplitudeDamping", gamma_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - gamma]}}, {{0, Sqrt[gamma]}, {0, 0}}}, args, "Label" -> "AmplitudeDamping"[gamma]]

QuantumChannel[{"GeneralizedAmplitudeDamping", gamma_, p_}, args___] :=
	quantumChannel[{
        Sqrt[p] {{1, 0}, {0, Sqrt[1 - gamma]}},
        Sqrt[p] {{0, Sqrt[gamma]}, {0, 0}},
        Sqrt[1 - p] {{Sqrt[1 - gamma], 0}, {0, 1}},
        Sqrt[1 - p] {{0, 0}, {Sqrt[gamma], 0}}
    },
        args,
        "Label" -> "GeneralizedAmplitudeDamping"[gamma, p]
    ]

QuantumChannel[{"PhaseDamping", lambda_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - lambda]}}, {{0, 0}, {0, Sqrt[lambda]}}}, args, "Label" -> "PhaseDamping"[lambda]]


QuantumChannel[{"BitFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, 1}, {1, 0}}}, args, "Label" -> "BitFlip"[p]]

QuantumChannel[{"PhaseFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p]*IdentityMatrix[2], Sqrt[1 - p]*{{1, 0}, {0, -1}}}, args, "Label" -> "PhaseFlip"[p]]

QuantumChannel[{"BitPhaseFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, -I}, {I ,0}}}, args, "Label" -> "BitPhaseFlip"[p]]

QuantumChannel[{"Depolarizing", p_}, args___] :=
    quantumChannel[{
        Sqrt[1 - 3 p / 4] PauliMatrix[0],
        Sqrt[p] (1 / 2) PauliMatrix[1],
        Sqrt[p] (1 / 2) PauliMatrix[2],
        Sqrt[p] (1 / 2) PauliMatrix[3]
    },
        args,
        "Label" -> "\[CapitalDelta]"[p]
    ]

