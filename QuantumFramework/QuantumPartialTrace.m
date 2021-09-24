Package["QuantumFramework`"]

PackageExport["QuantumPartialTrace"]


QuantumPartialTrace[qb_QuditBasis, qudits_List] := QuditBasis[
    DeleteDuplicates[#["Delete", List /@ qudits] & /@ qb["Names"]],
    ArrayReshape[
        TensorContract[ArrayReshape[qb["Elements"], Join[qb["Dimensions"], qb["Dimensions"]]], Thread[{qudits, qudits + qb["NameRank"]}]],
        Prepend[Delete[qb["Dimensions"], qudits], Times @@ Delete[qb["Dimensions"], qudits]]
    ]
]

QuantumPartialTrace[qb_QuantumBasis, inputs_List, outputs_List] :=
    QuantumBasis[qb,
        "Input" -> qb["Input"]["Delete", inputs],
        "Output" -> qb["Output"]["Delete", outputs]
    ]

