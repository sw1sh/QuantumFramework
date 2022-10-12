Package["Wolfram`QuantumFramework`"]



$QuantumCircuitOperatorProperties = {
    "Operators", "Diagram", "Gates", "Orders", "CircuitOperator", "QiskitCircuit", "Label",
    "Depth", "Arity", "Width", "TensorNetwork"
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
    If[ TrueQ[$QuantumFrameworkPropCache] && ! MemberQ[{"Elements", "Diagram", "Qiskit", "QiskitCircuit"}, propName[prop]],
        QuantumCircuitOperatorProp[qds, prop, args] = result,
        result
    ] /;
        !FailureQ[Unevaluated @ result] && (!MatchQ[Unevaluated @ result, _QuantumCircuitOperatorProp] || Message[QuantumCircuitOperator::undefprop, prop])
]

QuantumCircuitOperatorProp[QuantumCircuitOperator[data_Association], key_String] /; KeyExistsQ[data, key] := data[key]

QuantumCircuitOperatorProp[qco_, "Operators"] := DeleteCases[qco["Elements"], _ ? BarrierQ]


QuantumCircuitOperatorProp[qco_, "Diagram", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[qco, opts, ImageSize -> Medium]

QuantumCircuitOperatorProp[qco_, "Icon", opts : OptionsPattern[Options[CircuitDraw]]] :=
    CircuitDraw[
        qco, opts,
        "ShowGateLabels" -> False, "ShowMeasurementWire" -> False, "WireLabels" -> None,
        "SubcircuitOptions" -> {"ShowLabel" -> False},
        ImageSize -> Tiny
    ]


Options[quantumCircuitCompile] = {Method -> Automatic}

quantumCircuitCompile[qco_QuantumCircuitOperator, OptionsPattern[]] :=
    Switch[
        OptionValue[Method],
        "Schrodinger" | "Schroedinger" | "Schrödinger",
        Fold[ReverseApplied[Construct], qco["Flatten"]["Operators"]],
        Automatic | "TensorNetwork",
        Which[
            qco["Eigenqudits"] > 0,
            QuantumMeasurementOperator[QuantumOperator[#, {Join[Range[- qco["Eigenqudits"] + 1, 0], qco["OutputOrder"]], qco["InputOrder"]}], qco["Target"]] &,
            qco["TraceQudits"] > 0,
            QuantumChannel[QuantumOperator[#, qco["Order"]]] &,
            True,
            QuantumOperator[#, qco["Order"]] &
        ] @ QuantumState[SparseArrayFlatten[#], QuantumBasis @@ Reverse @ TakeDrop[TensorDimensions[#], - qco["Arity"]], "Label" -> qco["Label"]] & @
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

QuantumCircuitOperatorProp[qco_, "Depth"] := Max[1, Counts[Catenate[Union @@@ qco["Orders"]]]]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "Width"] := Max[1, #2 - Min[Max[#1, 1], 1] + 1] & @@ MinMax[{qco["InputOrder"], qco["OutputOrder"]}]

QuantumCircuitOperatorProp[qco_, "InputDimensions"] :=
    (q |-> #["InputDimensions"][[ q /. #["InputOrderQuditMapping"] ]] & @
        SelectFirst[qco["Operators"], op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Times @@ qco["InputDimensions"]

QuantumCircuitOperatorProp[qco_, "OutputDimensions"] :=
    (q |-> #["OutputDimensions"][[ q /. #["OutputOrderQuditMapping"] ]] & @
        SelectFirst[Reverse @ qco["Operators"], op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputDimension"] := Times @@ qco["OutputDimensions"]

QuantumCircuitOperatorProp[qco_, "Input"] :=
    QuantumTensorProduct[
        (q |-> #["Input"]["Extract", {q /. #["InputOrderQuditMapping"]}] & @
            SelectFirst[qco["Operators"], op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]
    ]

QuantumCircuitOperatorProp[qco_, "Output"] :=
    QuantumTensorProduct[
        (q |-> #["Output"]["Extract", {q /. #["OutputOrderQuditMapping"]}] & @
            SelectFirst[qco["Operators"], op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]
    ]

QuantumCircuitOperatorProp[qco_, "Measurements"] := Count[qco["Operators"], _ ? QuantumMeasurementOperatorQ]

QuantumCircuitOperatorProp[qco_, "Channels"] := Count[qco["Operators"], _ ? QuantumChannelQ]

QuantumCircuitOperatorProp[qco_, "Target"] := Join @@ (
    #["Target"] & /@ Select[qco["Operators"], QuantumMeasurementOperatorQ]
)

QuantumCircuitOperatorProp[qco_, "Targets"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "TargetOrder"] := qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "TargetArity"] := Length @ qco["Target"]

QuantumCircuitOperatorProp[qco_, "Eigenqudits"] := Total[#["Eigenqudits"] & /@ Select[qco["Operators"], QuantumMeasurementOperatorQ]]

QuantumCircuitOperatorProp[qco_, "TraceOrder"] := Union @@ (#["TraceOrder"] & /@ Select[qco["Operators"], QuantumChannelQ])

QuantumCircuitOperatorProp[qco_, "TraceQudits"] := Total[#["TraceQudits"] & /@ Select[qco["Operators"], QuantumChannelQ]]


QuantumCircuitOperatorProp[qco_, "QiskitCircuit" | "Qiskit"] := QuantumCircuitOperatorToQiskit[qco]

QuantumCircuitOperatorProp[qco_, "Stats" | "Statistics"] := Counts[#["Arity"] & /@ qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Flatten", n : _Integer ? NonNegative | Infinity : Infinity] :=
    QuantumCircuitOperator[
        If[ n === Infinity,
            FixedPoint[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["Elements"], {1}], 1] &, qco["Elements"]],
            Nest[Flatten[Replace[#, c_ ? QuantumCircuitOperatorQ :> c["Elements"], {1}], 1] &, qco["Elements"], n]
        ],
        qco["Label"]
    ]

QuantumCircuitOperatorProp[qco_, "Sort"] := QuantumCircuitOperator[#["Sort"] & /@ qco["Operators"]]


QuantumCircuitOperatorProp[qco_, "Shift", n : _Integer ? NonNegative : 1] :=
    QuantumCircuitOperator[#["Shift", n] & /@ qco["Operators"], qco["Label"]]


QuantumCircuitOperatorProp[qco_, "Dagger"] :=
    QuantumCircuitOperator[#["Dagger"] & /@ Reverse @ qco["Operators"], SuperDagger[qco["Label"]]]

QuantumCircuitOperatorProp[qco_, prop : "Conjugate" | "Dual"] :=
    QuantumCircuitOperator[#[prop] & /@ qco["Operators"], SuperStar[qco["Label"]]]


QuantumCircuitOperatorProp[qco_, "Normal"] :=
    QuantumCircuitOperator[Which[QuantumMeasurementOperatorQ[#], #["SuperOperator"], QuantumChannelQ[#], #["QuantumOperator"], True, #] & /@ qco["Operators"], qco["Label"]]

QuantumCircuitOperatorProp[qco_, "TensorNetwork", opts : OptionsPattern[QuantumTensorNetwork]] := QuantumTensorNetwork[qco["Flatten"], opts]


QuantumCircuitOperatorProp[qco_, "QASM"] :=
    Enclose[StringTemplate["OPENQASM 3.0;\nqubit[``] q;\nbit[``] c;\n"][qco["Width"], qco["Targets"]] <>
        StringRiffle[ConfirmBy[#["QASM"], StringQ] & /@ qco["Flatten"]["Operators"], "\n"]]

(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[Last[qco["Operators"]]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

