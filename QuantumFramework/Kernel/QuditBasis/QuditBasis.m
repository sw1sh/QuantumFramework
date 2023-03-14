Package["Wolfram`QuantumFramework`"]

PackageExport["QuditBasis"]
PackageExport["QuditName"]

PackageScope["QuditBasisQ"]



QuditBasis::inconsistentNames = "element names should have the same length";

QuditBasis::inconsistentElements = "elements should have the same dimensions";

QuditBasis::dependentElements = "elements should be linearly independent";



quditBasisQ[QuditBasis[representations_Association]] := Enclose[
Module[{
},
    ConfirmAssert[AllTrue[Keys @ representations, MatchQ[{_QuditName, _Integer ? Positive}]]];
    ConfirmAssert[And @@ ResourceFunction["KeyGroupBy"][
        Select[representations, TensorRank[#] > 0 &],
        Last,
        Apply[Equal] @* Map[Dimensions]
    ]];
    True
],
False &
]

quditBasisQ[___] := False


QuditBasisQ[qb : QuditBasis[_]] := System`Private`HoldValidQ[qb]

QuditBasisQ[___] := False

qb_QuditBasis /; System`Private`HoldNotValidQ[qb] && quditBasisQ[Unevaluated @ qb] := System`Private`HoldSetValid[qb]


QuditBasis[] := QuditBasis[{QuditName[]}, {1}]

QuditBasis[$QuditZero] := QuditBasis[0]

QuditBasis[{}] := QuditBasis[]

QuditBasis[qb_QuditBasis] := qb

QuditBasis[assoc_Association] /; ! AllTrue[Keys[assoc], MatchQ[{_QuditName, _Integer ? Positive}]] :=
    QuditBasis[
        KeyMap[{QuditName[#], 1} &, assoc]
    ]

QuditBasis[names : {Except[_Integer | (name_String | {name_String, ___} /; MemberQ[$QuditBasisNames, name])] ..}] :=
    QuditBasis[names, If[Length[names] == 1, {1}, identityMatrix[Length[names]]]]

QuditBasis[names_List, elements_ ? ArrayQ] :=
    QuditBasis[AssociationThread[names, Normal @ elements]]

QuditBasis[elements_Association] /; Not @ AllTrue[elements, SparseArrayQ[#] || AtomQ[#] || emptyTensorQ[#] &] :=
    QuditBasis[Map[If[AtomQ[#], #, SparseArray[#]] &, elements]]

(* QuditBasis[elements_Association] /; !OrderedQ[Reverse /@ Keys[elements]] :=
    QuditBasis[KeySortBy[elements, Reverse]] *)


(* tensor product of multiple parameter basis *)

QuditBasis[{name_String, params_List}, args___] := Enclose[QuantumTensorProduct @@ (ConfirmBy[QuditBasis[{name, #}, args], QuditBasisQ] & /@ params)]

QuditBasis[params_List] := Enclose[QuantumTensorProduct @@ (ConfirmBy[QuditBasis[#], QuditBasisQ] & /@ params)]


(* multiplicity *)

QuditBasis[
    name : _String | {_String, PatternSequence[] | PatternSequence[Except[_List], ___]} | _Integer | _Association,
    multiplicity_Integer ? NonNegative, args___] :=
    QuditBasis[QuditBasis[name, args], multiplicity, args]

QuditBasis[dimension_Integer, multiplicity_Integer ? NonNegative] := QuditBasis[QuditBasis[dimension], multiplicity]

QuditBasis[qb_QuditBasis ? QuditBasisQ, multiplicity_Integer ? NonNegative] :=
    If[multiplicity > 0, QuantumTensorProduct[Table[qb, multiplicity]], QuditBasis[]]


(* basis cast *)

QuditBasis[source_QuditBasis, target_QuditBasis] := If[
    target["Dimension"] >= source["Dimension"],
    target["TakeDimension", source["Dimension"]],
    QuantumTensorProduct[target, source["DropDimension", target["Dimension"]]]
]

QuditBasis[source_QuditBasis -> target_QuditBasis] := QuditBasis[
    AssociationThread[Keys @ target["Representations"],
    Values @ source["Representations"]]
]


(* equality *)

QuditBasis /: Equal[qb__QuditBasis ? QuditBasisQ] :=
    Thread[Equal @@ (Chop @ SetPrecision[SparseArrayFlatten @ Values @ #["RemoveIdentities"]["Sort"]["Representations"], $MachinePrecision - 2] & /@ {qb})]


(* addition *)

QuditBasis /: Plus[qb__QuditBasis ? QuditBasisQ] := Module[{
    repr = MapIndexed[{b, i} |-> KeyValueMap[#1[[2]] -> {#1[[1]]["Name"], SparseArrayFlatten @ #2, First[i]} &, b["Representations"]], {qb}],
    dims = #["ElementDimensions"] & /@ {qb},
    maxRank
},
    maxRank = Max[Length /@ dims];
    dims = PadRight[#, maxRank, 1] & /@ dims;
    QuditBasis @ Association @ KeyValueMap[
        {k, vs} |-> MapThread[{QuditName[#1], k} -> #2 &] @ {
            Thread[Subscript[vs[[All, 1]], vs[[All, 3]]]],
            MapThread[
                PadRight[
                    PadLeft[
                        Replace[#1, x_ ? NumericQ :> {x}],
                        Total @ Take[dims, #2][[All, k]]
                    ],
                    Total @ dims[[All, k]]
                ] &,
                {vs[[All, 2]], vs[[All, 3]]}
            ]
        },
        Merge[repr, Identity]
    ]
]


(* formatting *)

QuditBasis /: MakeBoxes[qb_QuditBasis ? QuditBasisQ, format : TraditionalForm] := With[{boxes = ToBoxes[Normal /@ qb["Association"], format]},
    InterpretationBox[boxes, qb]
]

QuditBasis /: MakeBoxes[qb_QuditBasis ? QuditBasisQ, format_] := With[{
    icon = If[
        qb["ElementDimension"] < 2 ^ 9,
        ComplexArrayPlot[
            Map[Replace[{x_ ? (Not @* NumericQ) :> BlockRandom[RandomComplex[], RandomSeeding -> Hash[x]], x_ :> N[x]}], qb["Matrix"], {2}],
            ImageSize -> Dynamic @ {Automatic, 3.5 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]},
            Frame -> False,
            FrameTicks -> None
        ],
        RawBoxes @ $SparseArrayBox
    ]
},
    BoxForm`ArrangeSummaryBox["QuditBasis", qb, icon,
    {
        {
            BoxForm`SummaryItem[{"Qudits: ", qb["Qudits"]}]
        },
        {
            BoxForm`SummaryItem[{"Dimension: ", qb["Dimension"]}]
        }
    },
    {
        {
            BoxForm`SummaryItem[{"Shape: ", qb["Shape"]}]
        },
        {
            BoxForm`SummaryItem[{"Element dimensions: ", qb["ElementDimensions"]}]
        }
    },
    format,
    "Interpretable" -> Automatic
    ]
]

