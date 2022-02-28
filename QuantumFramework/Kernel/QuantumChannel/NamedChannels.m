Package["Wolfram`QuantumFramework`"]



$QuantumChannelNames = {
    "BitFlip", "PhaseFlip", "BitPhaseFlip", "Depolarize",
    "AmplitudeDamping", "GeneralizedAmplitudeDamping",
    "PhaseDamping"
}


quantumChannel[args___] := With[{qo = QuantumOperator[args]}, QuantumChannel @
    QuantumOperator[
        qo,
        "Output" -> QuditBasis @ KeyMap[Replace[{qn_QuditName, 1} :> {QuditName[Subscript["K", qn["Name"]]], 1}]] @
            qo["Output"]["Representations"]
    ]
]

QuantumChannel[{"AmplitudeDamping", gamma_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - gamma]}}, {{0, Sqrt[gamma]}, {0, 0}}}, args]

QuantumChannel[{"GeneralizedAmplitudeDamping", gamma_, p_}, args___] :=
	quantumChannel[{
        Sqrt[p] {{1, 0}, {0, Sqrt[1 - gamma]}},
        Sqrt[p] {{0, Sqrt[gamma]}, {0, 0}},
        Sqrt[1 - p] {{Sqrt[1 - gamma], 0}, {0, 1}},
        Sqrt[1 - p] {{0, 0}, {Sqrt[gamma], 0}}
    },
        args
    ]

QuantumChannel[{"PhaseDamping", lambda_}, args___] :=
    quantumChannel[{{{1, 0}, {0, Sqrt[1 - lambda]}}, {{0, 0}, {0, Sqrt[lambda]}}}, args]


QuantumChannel[{"BitFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, 1}, {1, 0}}}, args]

QuantumChannel[{"PhaseFlip", p_}, args___] :=
    quantumChannel[{Sqrt[p]*IdentityMatrix[2], Sqrt[1 - p]*{{1, 0}, {0, -1}}}, args]

QuantumChannel["BitPhaseFlip", p_, args___] :=
    quantumChannel[{Sqrt[p] IdentityMatrix[2], Sqrt[1 - p] {{0, -I}, {I ,0}}}, args]

QuantumChannel[{"Depolarize", p_}, args___] :=
    quantumChannel[{
        Sqrt[(1 - 3 p) / 4] IdentityMatrix[2],
        Sqrt[p] (1 / 2) {{0, 1}, {1, 0}},
        Sqrt[p] (1 / 2) {{0, -I}, {I, 0}},
        Sqrt[p] (1 / 2) {{1, 0}, {0, -1}}
    },
        args
    ]

