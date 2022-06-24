Package["Wolfram`QuantumFramework`"]



$QuantumCircuitOperatorProperties = {
     "Operators", "Diagram", "Gates", "Orders", "CircuitOperator", "QiskitCircuit", "Label",
     "Depth", "Arity", "Width"
};


QuantumCircuitOperator["Properties"] := Sort @ $QuantumCircuitOperatorProperties

QuantumCircuitOperatorProp[qco_, "Properties"] := Enclose @ Union @ Join[
    QuantumCircuitOperator["Properties"],
    ConfirmBy[Last[qco["Operators"]], QuantumFrameworkOperatorQ]["Properties"]
]


qds_QuantumCircuitOperator["ValidQ"] := QuantumCircuitOperatorQ[qds]


QuantumCircuitOperator::undefprop = "property `` is undefined for this circuit";

(qds_QuantumCircuitOperator[prop_ ? propQ, args___]) /; QuantumCircuitOperatorQ[qds] := With[{
    result = QuantumCircuitOperatorProp[qds, prop, args]
},
    If[ TrueQ[$QuantumFrameworkPropCache] && ! MemberQ[{"Operators", "Diagram", "Qiskit", "QiskitCircuit"}, propName[prop]],
        QuantumCircuitOperatorProp[qds, prop, args] = result,
        result
    ] /;
        !FailureQ[Unevaluated @ result] && (!MatchQ[Unevaluated @ result, _QuantumCircuitOperatorProp] || Message[QuantumCircuitOperator::undefprop, prop])
]

QuantumCircuitOperatorProp[QuantumCircuitOperator[data_Association], key_String] /; KeyExistsQ[data, key] := data[key]

QuantumCircuitOperatorProp[qco_, "Diagram", opts : OptionsPattern[Join[Options[drawGateGraphics], Options[Graphics]]]] := Module[{
    labels, indices, graphics,
    width, height,
    sizes,
    imageWidth, imageHeight,
    scale = 0.001
    (* scale = Dynamic[0.26 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]] *)
},
    {labels, indices, graphics} = drawGateGraphics[qco["Operators"],
        FilterRules[{opts}, Options[drawGateGraphics]]
    ];
    (* graphics = graphics /. {Thickness[t_] :> Thickness[100 scale t], Arrowheads[s_] :> Arrowheads[0.5 scale s]}; *)
    width = Max[indices];
    height = qco["Arity"];
    sizes = Most @ Rasterize[#, "BoundingBox"] & /@ labels;
    imageWidth = Min[width Max[sizes[[All, 1]] + 1, 128], 512];
    imageHeight = Min[height Max[sizes[[All, 2]] + 1, 128], 512];
    Show[graphics,
        FilterRules[{opts}, Options[Graphics]],
        ImageSize -> If[width > height, {imageWidth, Automatic}, {Automatic, imageHeight}]
    ]
]

Options[quantumCircuitCompile] = {Method -> Automatic}

quantumCircuitCompile[qco_QuantumCircuitOperator, OptionsPattern[]] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qco["Flatten"]["Operators"]],
        Automatic | "TensorNetwork",
        QuantumOperator[QuantumState[SparseArrayFlatten[#], QuantumBasis @@ TakeDrop[TensorDimensions[#], qco["Arity"]], "Label" -> qco["Label"]], qco["Order"]] & @
            With[{tn = VertexDelete[qco["TensorNetwork"], 0]},
                Transpose[ContractTensorNetwork[tn], Ordering @ OrderingBy[TensorNetworkFreeIndices[tn], Replace[{Superscript[_, x_] :> {0, x}, Subscript[_, x_] :> {1, x}}]]]
            ],
        _,
        $Failed
    ]

QuantumCircuitOperatorProp[qco_, "QuantumOperator" | "CircuitOperator" | "Compile", opts : OptionsPattern[quantumCircuitCompile]] := quantumCircuitCompile[qco, opts]

QuantumCircuitOperatorProp[qco_, "Gates"] := Length @ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "InputOrders"] := #["InputOrder"] & /@ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "InputOrder"] := Union @@ qco["InputOrders"]

QuantumCircuitOperatorProp[qco_, "OutputOrders"] := #["OutputOrder"] & /@ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "OutputOrder"] := Union @@ qco["OutputOrders"]

QuantumCircuitOperatorProp[qco_, "Orders"] := Thread[{qco["OutputOrders"], qco["InputOrders"]}]

QuantumCircuitOperatorProp[qco_, "Order"] := {qco["OutputOrder"], qco["InputOrder"]}

QuantumCircuitOperatorProp[qco_, "Depth"] := Max @ Counts[Catenate[Union @@@ qco["Orders"]]]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "Width"] := #2 - Min[#1, 1] + 1 & @@ MinMax[{qco["InputOrder"], qco["OutputOrder"]}]

QuantumCircuitOperatorProp[qco_, "InputDimensions"] :=
    (q |-> #["InputDimensions"][[ q /. #["InputOrderQuditMapping"] ]] & @
        SelectFirst[qco["Operators"], op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Times @@ qco["InputDimensions"]

QuantumCircuitOperatorProp[qco_, "OutputDimensions"] :=
    (q |-> #["OutputDimensions"][[ q /. #["OutputOrderQuditMapping"] ]] & @
        SelectFirst[Reverse @ qco["Operators"], op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputDimension"] := Times @@ qco["OutputDimensions"]

QuantumCircuitOperatorProp[qco_, "Measurements"] := Count[qco["Operators"], _ ? QuantumMeasurementOperatorQ]

QuantumCircuitOperatorProp[qco_, "Target"] := DeleteDuplicates[Join @@ (#["Target"] & /@ Select[qco["Operators"], QuantumMeasurementOperatorQ])]

QuantumCircuitOperatorProp[qco_, "Targets"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "TargetOrder"] := qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "TargetArity"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "QiskitCircuit" | "Qiskit"] := QuantumCircuitOperatorToQiskit[qco]

QuantumCircuitOperatorProp[qco_, "Label"] := "Q"

QuantumCircuitOperatorProp[qco_, "Stats" | "Statistics"] := Counts[#["Arity"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Flatten", n : _Integer ? NonNegative | Infinity : Infinity] :=
    QuantumCircuitOperator[
        If[ n === Infinity,
            FixedPoint[Flatten[Map[Replace[c_ ? QuantumCircuitOperatorQ :> c["Operators"]], #], 1] &, qco["Operators"]],
            Nest[Flatten[Map[Replace[c_ ? QuantumCircuitOperatorQ :> c["Operators"]], #], 1] &, qco["Operators"], n]
        ]
    ]

QuantumCircuitOperatorProp[qco_, "Sort"] := QuantumCircuitOperator[#["Sort"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Dagger"] :=
    QuantumCircuitOperator[#["Dagger"] & /@ Reverse @ qco["Operators"], Superscript[qco["Label"], "\[Dagger]"]]


QuantumCircuitOperatorProp[qco_, "TensorNetwork", opts : OptionsPattern[QuantumTensorNetwork]] := QuantumTensorNetwork[qco["Flatten"], opts]


QuantumCircuitOperatorProp[qco_, "QASM"] :=
    Enclose[StringTemplate["OPENQASM 3.0;\nqubit[``] q;\nbit[``] c;\n"][qco["Width"], qco["Targets"]] <>
        StringRiffle[ConfirmBy[#["QASM"], StringQ] & /@ qco["Flatten"]["Operators"], "\n"]]

(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[Last[qco["Operators"]]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

