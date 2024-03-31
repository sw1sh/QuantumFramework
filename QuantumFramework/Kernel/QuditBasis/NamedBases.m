Package["Wolfram`QuantumFramework`"]

PackageScope["$QuditBasisNames"]
PackageScope["$QuditPhaseSpaceBasisNames"]



$QuditBasisNames = {
    "Computational",
    "PauliX", "PauliY", "PauliZ",
    "Identity", "I", "X", "Y", "Z",
    "JX", "JY", "JZ",
    "Bell",
    "Fourier",
    "Schwinger", "Pauli", "Dirac", "Wigner", "WignerMIC", "GellMann",
    "Wootters", "Feynman",
    "Tetrahedron", "RandomMIC"
}

$QuditPhaseSpaceBasisNames = {"Wigner", "Wootters", "Tetrahedron", "Pauli", "GellMann", "Feynman", "WignerMIC", "RandomMIC"}

$QuditBasisCache = <||>
$QuditBasisCaching = True

QuditBasis[args___] /; $QuditBasisCaching && FreeQ[{args}, "RandomMIC"] := Lookup[
    $QuditBasisCache, Key[{args}],
    $QuditBasisCache[{args}] = Block[{$QuditBasisCaching = False}, QuditBasis[args]]
]


QuditBasis[1, args___] := QuditBasis[args]

QuditBasis[dimension_Integer, args___] := QuditBasis[{"Computational", dimension}, args]

QuditBasis["Computational" | "Identity" | "I", args___] := QuditBasis[{"Computational", 2}, args]

QuditBasis[{"Computational" | "Identity" | "I", 0}, ___] := QuditBasis[{$QuditZero}, {{}}]

QuditBasis[{"Computational" | "Identity" | "I", dimension_Integer ? Positive}, args___] :=
    QuditBasis[QuditBasis[Range[dimension] - 1, identityMatrix[dimension]], args]


QuditBasis["Bell", args___] := QuditBasis[
    AssociationThread[{
            Superscript["\[CapitalPhi]", "-"],
            Superscript["\[CapitalPsi]", "-"],
            Superscript["\[CapitalPsi]", "+"],
            Superscript["\[CapitalPhi]", "+"]
        },
        (1 / Sqrt[2]) {{1, 0, 0, -1}, {0, 1, -1, 0}, {0, 1, 1, 0}, {1, 0, 0, 1}}
    ],
    args
]


QuditBasis[name : "X" | "Y" | "Z", args___] := QuditBasis["Pauli" <> name, args]

QuditBasis[{name : "X" | "Y" | "Z", dim_Integer : 2}, args___] := QuditBasis[{"Pauli" <> name, dim}, args]

QuditBasis[name : "PauliX" | "PauliY" | "PauliZ", args___] := QuditBasis[{name, 2}, args]

QuditBasis[{name : "PauliX" | "PauliY" | "PauliZ", dim_Integer : 2}, args___] := With[{
    es = eigensystem[pauliMatrix[name /. {"PauliX" -> 1, "PauliY" -> 2, "PauliZ" -> 3}, dim], "Normalize" -> True, "Sort" -> True]
},
    QuditBasis[
        AssociationThread[
            MapIndexed[
                Subscript[
                    Replace[StringDelete[name, "Pauli"], {"X" -> "\[ScriptX]", "Y" -> "\[ScriptY]", "Z" -> "\[ScriptZ]"}],
                    Interpretation[If[dim == 2, Replace[#2[[1]], {1 -> "+", 2 -> "\[Minus]"}], #2[[1]] - 1], #]
                ] &,
                First[es]
            ],
            Last[es]
        ],
        args
    ]
]

QuditBasis[name : "JX" | "JY" | "JZ", args___] := QuditBasis[{name, 1 / 2}, args]

QuditBasis[{name : "JX" | "JY" | "JZ", j_ : 1 / 2}, args___] /; IntegerQ[2 j] := With[{
    es = eigensystem[spinMatrix[name /. {"JX" -> 1, "JY" -> 2, "JZ" -> 3}, 2 j + 1], "Normalize" -> True, "Sort" -> True]
},
    QuditBasis[
        AssociationThread[
            MapIndexed[
                Subsuperscript[
                    "J",
                    Interpretation[If[j == 1 / 2, Replace[#2[[1]], {1 -> "\[UpArrow]", 2 -> "\[DownArrow]"}], InputForm[- j + #2[[1]] - 1]], #],
                    ToLowerCase @ StringDelete[name, "J"]
                ] &,
                First[es]
            ],
            Last[es]
        ],
        args
    ]
]


QuditBasis["Fourier"] := QuditBasis[{"Fourier", 2}]

QuditBasis[{"Fourier", qb_ ? QuditBasisQ}, args___] := With[{dimension = qb["Dimension"], elements = SparseArrayFlatten /@ qb["Elements"]},
    QuditBasis[
        AssociationThread[
            Subscript["F", #] & /@ Range[dimension],
            Map[i |->
                1 / Sqrt[dimension] *
                    Total @ MapIndexed[{p, idx} |-> With[{j = idx[[1]] - 1}, Exp[I 2 Pi i j / dimension] p], elements],
                Range[0, dimension - 1]
            ]
        ],
        args
    ]
]

QuditBasis[{"Fourier", basisArgs___}, args___] := QuditBasis[{"Fourier", QuditBasis[basisArgs]}, args]


QuditBasis["Schwinger"] := QuditBasis[{"Schwinger", 2}]

QuditBasis[{"Schwinger", dimension_Integer ? Positive}, args___] := QuditBasis[
    AssociationThread[
        Subscript["S", Row[#]] & /@ Tuples[Range[0, dimension - 1], 2],
        Flatten /@ (
            Dot[
                MatrixPower[RotateLeft[IdentityMatrix[dimension]], #[[1]]],
                MatrixPower[((Exp[I 2 Pi / dimension]) ^ #) & /@ Range[0, dimension - 1] IdentityMatrix[dimension], #[[2]]]
            ]
        ) & /@ Partition[Flatten[Table[{i, j}, {i, 0, dimension - 1}, {j, 0, dimension - 1}]], 2]],
    args
]

QuditBasis["Pauli", args___] := QuditBasis[
    AssociationThread[{Subscript["\[Sigma]", "0"],
        Subscript["\[Sigma]", "1"], Subscript["\[Sigma]", "2"],
        Subscript["\[Sigma]", "3"]},
        {{{1, 0}, {0, 1}}, {{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}}
    ],
    args
]

QuditBasis["Dirac", args___] := Module[{
    pauliBasis1, pauliBasis2, pauliBasis3, pauliBasis4,
    gamma1, gamma2, gamma3, gamma4
},
    pauliBasis1 = {{1, 0}, {0, 1}};
    pauliBasis2 = {{0, 1}, {1, 0}};
    pauliBasis3 = {{0, -I}, {I, 0}};
    pauliBasis4 = {{1, 0}, {0, -1}};
    gamma1 = KroneckerProduct[pauliBasis4, pauliBasis1];
    gamma2 = KroneckerProduct[pauliBasis3, pauliBasis2];
    gamma3 = KroneckerProduct[pauliBasis3, pauliBasis3];
    gamma4 = KroneckerProduct[pauliBasis3, pauliBasis4];
    QuditBasis[AssociationThread[
        Subscript["\[Epsilon]", #] & /@ Range[0, 15],
        {
            IdentityMatrix[4],
            KroneckerProduct[pauliBasis4, pauliBasis1],
            KroneckerProduct[pauliBasis3, pauliBasis2],
            KroneckerProduct[pauliBasis3, pauliBasis3],
            KroneckerProduct[pauliBasis3, pauliBasis4],
            gamma1 . gamma2, gamma1 . gamma3, gamma1 . gamma4, gamma2 . gamma3, gamma2 . gamma4, gamma3 . gamma4,
            -I * (gamma2 . gamma3 . gamma4), I * (gamma1 . gamma3 . gamma4), -I * (gamma1 . gamma2 . gamma4), I * (gamma1 . gamma2 . gamma3),
            gamma1 . gamma2 . gamma3 . gamma4
       }],
       args
    ]
]


QuditBasis["Wigner", args___] := QuditBasis[{"Wigner", 2}, args]

QuditBasis[{"Wigner", qb_QuditBasis /; QuditBasisQ[qb], opts : OptionsPattern[WignerBasis]}, args___] :=
    QuditBasis[WignerBasis[qb, opts], args]

QuditBasis[{"Wigner", basisArgs___, opts : OptionsPattern[]}, args___] := QuditBasis[{"Wigner", QuditBasis[basisArgs], opts}, args]


QuditBasis[{"WignerMIC", args___}, opts___] := QuditBasis[QuantumWignerMICBasis[args], opts]

QuditBasis[{"GellMann", d : _Integer ? Positive : 2}] := QuditBasis[
    Subscript["\[ScriptCapitalG]", #] & /@ Range[d ^ 2],
    With[{povm = Normal /@ GellMannMICPOVM[d]},
        Inverse[Outer[Tr @* Dot, povm, povm, 1]] . povm // Simplify
    ]
]


WoottersBasis[d_] := With[{w = Exp[2 Pi I / d]}, 
    Catenate @ Table[
        Sum[w ^ ((d - 1) k l / 2 + q  l - p  k) MatrixPower[pauliMatrix[1, d], k] . MatrixPower[pauliMatrix[3, d], l], {k, 0, d - 1}, {l, 0, d - 1}] / d,
        {p, 0, d - 1}, {q, 0, d - 1}
    ]
]

QuditBasis[{"Wootters", d : _Integer ? Positive : 2}] := With[{factors = Catenate[Table @@@ FactorInteger[d]]},
    Simplify @ QuantumTensorProduct[QuditBasis[Subscript["\[ScriptCapitalW]", Row[{##}]] & @@@ Tuples[Range[0, # - 1], 2], WoottersBasis[#]] & /@ factors]
]

QuditBasis[{"Feynman", d : _Integer ? Positive : 2}] :=
    QuditBasis[Subscript["\[ScriptCapitalF]", #] & /@ Range[d ^ 2], WoottersBasis[d]]


QuditBasis[{"RandomMIC", d : _Integer ? Positive : 2, methodOpts : OptionsPattern[]}] := Enclose @ With[{
    povm = Normal /@ ConfirmBy[
        Replace[OptionValue[{methodOpts, Method -> "Haar"}, Method], {name_, args___} | name_ :>
            Switch[name, "Haar", RandomHaarPOVM, "Bloch", RandomBlochMICPOVM][d, args]],
        TensorQ
    ]
},
    QuditBasis[
        Subscript["\[ScriptCapitalR]", #] & /@ Range[Length[povm]],
        Inverse[Outer[Tr @* Dot, povm, povm, 1]] . povm // Chop
    ]
]

QuditBasis[{"Tetrahedron", args___}] := QuditBasis[
    Subscript["\[ScriptCapitalT]", #] & /@ Range[4],
    With[{povm = Normal /@ QuantumMeasurementOperator["TetrahedronSICPOVM"[args]]["POVMElements"]},
        Inverse[Outer[Tr @* Dot, povm, povm, 1]] . povm // Simplify
    ]
]


QuditBasis[pauliString_String] := With[{chars = Characters[pauliString]},
    QuditBasis[chars] /; Length[chars] > 1 && ContainsOnly[chars, {"I", "X", "Y", "Z"}]
]

QuditBasis[pauliString_String[dimension_Integer ? Positive]] := With[{chars = Characters[pauliString]},
    QuditBasis[Through[chars[dimension]]] /; Length[chars] > 1 && ContainsOnly[chars, {"I", "X", "Y", "Z"}]
]

QuditBasis[nameArg_ ? nameQ, args___] /; ! FreeQ[nameArg, _String ? (StringContainsQ["Basis"])] :=
    QuditBasis[nameArg /. name_String :> StringDelete[name, "Basis"], args]

QuditBasis[name_String[args___] | name_String, opts___] /; MemberQ[$QuditBasisNames, name] := QuditBasis[{name, args}, opts]

