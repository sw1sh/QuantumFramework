Package["QuantumFramework`"]

PackageExport["QuantumEntangledQ"]



QuantumEntangledQ[qs_ ? QuantumStateQ] := QuantumEntangledQ[qs, {{1}, {2}}]

QuantumEntangledQ[qs_ ? QuantumStateQ, biPartition_List] :=
    Module[ {passiveSubsystems, relevantState, reducedDensityMatrix, partialTransposeMatrix},

        If[ Length[Join @@ biPartition] < qs["Qudits"],

            passiveSubsystems = ReverseSort[Complement[Range[qs["Qudits"]], Join @@ biPartition]];
            relevantState = QuantumPartialTrace[qs, passiveSubsystems],

            passiveSubsystems = {};
            relevantState = qs
        ];

        If[ relevantState["PureStateQ"],

            reducedDensityMatrix = QuantumPartialTrace[qs, ReverseSort[Join[First[biPartition], passiveSubsystems]]]["DensityMatrix"];
            Re[Sqrt[2 (1 - Tr[reducedDensityMatrix . reducedDensityMatrix])]] > 0,

            partialTransposeMatrix = relevantState[{"Transpose", Take[Ordering[biPartition], 1]}]["DensityMatrix"];
            If[ Re[Log[2, Total[SingularValueList[partialTransposeMatrix]]]] > 0,
                True,

                If[ relevantState["Qudits"] == 2 && relevantState["Dimension"] == 2,
                    False,
                    Indeterminate
                ]
            ]
        ]
    ]

