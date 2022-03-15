Package["Wolfram`QuantumFramework`"]

PackageScope["$QuantumChannelNames"]



$QuantumChannelNames = {
    "BitFlip", "PhaseFlip", "BitPhaseFlip", "Depolarize",
    "AmplitudeDamping", "GeneralizedAmplitudeDamping",
    "PhaseDamping"
}


quantumChannel[args___] := With[{qo = QuantumOperator[args]}, QuantumChannel @
    QuantumOperator[
        qo,
        {Automatic, qo["InputOrder"]},
        "Output" -> QuditBasis @ KeyMap[Replace[{qn_QuditName, 1} :> {QuditName[Subscript["K", qn["Name"]]], 1}]] @
            qo["Output"]["Representations"]
    ]
]

QuantumChannel[{"AmplitudeDamping", gamma_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - gamma]}}, {{0, Sqrt[gamma]}, {0, 0}}}, args, "Label" -> "AmplitudeDamping"]

QuantumChannel[{"GeneralizedAmplitudeDamping", gamma_, p_}, args___] :=
	quantumChannel[{
        Sqrt[p] {{1, 0}, {0, Sqrt[1 - gamma]}},
        Sqrt[p] {{0, Sqrt[gamma]}, {0, 0}},
        Sqrt[1 - p] {{Sqrt[1 - gamma], 0}, {0, 1}},
        Sqrt[1 - p] {{0, 0}, {Sqrt[gamma], 0}}
    },
        args,
        "Label" -> "GeneralizedAmplitudeDamping"
    ]

QuantumChannel[{"PhaseDamping", lambda_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - lambda]}}, {{0, 0}, {0, Sqrt[lambda]}}}, args, "Label" -> "PhaseDamping"]


QuantumChannel[{"BitFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, 1}, {1, 0}}}, args, "Label" -> "BitFlip"]

QuantumChannel[{"PhaseFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p]*IdentityMatrix[2], Sqrt[1 - p]*{{1, 0}, {0, -1}}}, args, "Label" -> "PhaseFlip"]

QuantumChannel[{"BitPhaseFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, -I}, {I ,0}}}, args, "Label" -> "BitPhaseFlip"]

QuantumChannel[{"Depolarize", p_}, args___] :=
    quantumChannel[{
        Sqrt[(1 - 3 p) / 4] IdentityMatrix[2],
        Sqrt[p] (1 / 2) {{0, 1}, {1, 0}},
        Sqrt[p] (1 / 2) {{0, -I}, {I, 0}},
        Sqrt[p] (1 / 2) {{1, 0}, {0, -1}}
    },
        args,
        "Label" -> "Depolarize"
    ]

