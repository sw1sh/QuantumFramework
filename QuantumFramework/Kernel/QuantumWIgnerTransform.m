Package["Wolfram`QuantumFramework`"]

PackageExport["QuantumWignerTransform"]



Options[QuantumWignerTransform] = {"Exact" -> True}

QuantumWignerTransform[qb_ ? QuditBasisQ, OptionsPattern[]] /; qb["Dimension"] == 1 := qb

QuantumWignerTransform[qb_ ? QuditBasisQ, OptionsPattern[]] := Module[{
    basisElements = qb["Association"], newDimensionality, positionBasis, momentumBasis, kernelElement
},
    newDimensionality = qb["Dimension"];
    positionBasis = {#} & /@ Normal[Flatten /@ basisElements];
    If[ !TrueQ[OptionValue["Exact"]],
        positionBasis = N @ positionBasis
    ];
    momentumBasis = Function[{dimension},
        {dimension + 1 ->
            (1 / Sqrt[Length[positionBasis]]) Total[
                (Exp[(I 2 Pi dimension #) / Length[positionBasis]] (Values @@ positionBasis[[# + 1]])) & /@
                Range[0, Length[positionBasis] - 1]
            ]
        }
    ] /@ Range[0, newDimensionality - 1] // Simplify;
    kernelElement = Simplify @ With[{
        eigensystem = Simplify @ ReverseSort[Rule @@@ Thread[
            Eigensystem @ Partition[
                Map[
                    Module[{q1 = First[First[#]], p1 = Last[First[#]], q2 = First[Last[#]], p2 = Last[Last[#]]},
                        Exp[(2 Pi I (q1 - q2) (p1 - p2)) / newDimensionality]
                    ] &,
                    Tuples[Tuples[Range[0, newDimensionality - 1], 2], 2]
                ],
                newDimensionality ^ 2
                ]
            ]
        ]
    },
        Dot[
            Transpose[Normalize /@ Values[eigensystem]],
            DiagonalMatrix[Sqrt[Keys[eigensystem]]],
            Inverse[Transpose[Normalize /@ Values[eigensystem]]]
        ]
    ];
    QuditBasis[AssociationThread[
        Subscript["W", Row @ #] & /@ Tuples[Range[0, newDimensionality - 1], 2],

        Simplify @ Flatten[
        Table[
            With[{
                labels = Thread[Tuples[Range[0, newDimensionality - 1], 2] -> Range[newDimensionality ^ 2]]
            },
                Sqrt[newDimensionality] Total @ Map[
                    With[{q1 = First[#], p1 = Last[#]},
                        Times[
                            Extract[kernelElement, ({{i, j}, #} /. labels)],
                            First[Flatten[(Conjugate[Values[momentumBasis[[p1 + 1]]]] . Transpose[Values[positionBasis[[q1 + 1]]]])]],
                            KroneckerProduct[First[Values[momentumBasis[[p1 + 1]]]], Conjugate[First[Values[positionBasis[[q1 + 1]]]]]]
                        ]
                    ] &,
                    Tuples[Range[0, newDimensionality - 1], 2]
            ]
            ],
            {i, 0, newDimensionality - 1}, {j, 0, newDimensionality - 1}
        ],
            {1, 2}
        ]
    ]]
]


QuantumWignerTransform[qb_ ? QuantumBasisQ, opts : OptionsPattern[]] /; qb["Picture"] =!= "PhaseSpace" :=
    QuantumBasis[QuantumWignerTransform[QuantumTensorProduct[qb["Output"], qb["Input"]], opts], "PhaseSpace"]


QuantumWignerTransform[qs_ ? QuantumStateQ, opts : OptionsPattern[]] := With[{
    basis = QuantumWignerTransform[qs["Basis"], opts],
    densityMatrix = qs["Computational"]["DensityMatrix"]
},
    QuantumState[
        QuantumState[
            Tr[densityMatrix . ArrayReshape[#, Times @@@ Partition[Dimensions[#], Length[#] / 2]]] / basis["Dimension"] & /@ basis["BasisElements"] // Simplify,
            QuantumBasis[basis["OutputDimensions"], basis["InputDimensions"]]
        ],
        basis
    ]
]


QuantumWignerTransform[qo_ ? QuantumOperatorQ, opts : OptionsPattern[]] := QuantumOperator[QuantumWignerTransform[qo["State"], opts]]

QuantumWignerTransform[qmo_ ? QuantumMeasurementOperatorQ, opts : OptionsPattern[]] :=
    QuantumMeasurementOperator[QuantumWignerTransform[qmo["Operator"], opts]]

