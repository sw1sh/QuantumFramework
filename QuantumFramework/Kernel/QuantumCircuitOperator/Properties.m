Package["Wolfram`QuantumFramework`"]



$QuantumCircuitOperatorProperties = {
     "Operators", "Diagram", "Gates", "Orders", "CircuitOperator", "QiskitCircuit"
};


QuantumCircuitOperator["Properties"] := $QuantumCircuitOperatorProperties

QuantumCircuitOperatorProp[qco_, "Properties"] := Enclose @ DeleteDuplicates @ Join[
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

QuantumCircuitOperatorProp[QuantumCircuitOperator[operators_], "Operators"] := operators

QuantumCircuitOperatorProp[qco_, "Diagram", opts : OptionsPattern[Join[Options[drawGateGraphics], Options[Graphics]]]] := Module[{
    labels, indices, graphics,
    width, height,
    sizes,
    imageWidth, imageHeight,
    scale = Dynamic[0.26 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]]
},
    {labels, indices, graphics} = drawGateGraphics[qco["Operators"],
        FilterRules[{opts}, Options[drawGateGraphics]]
    ];
    graphics = graphics /. {Thickness[t_] :> Thickness[100 scale t], Arrowheads[s_] :> Arrowheads[0.1 scale s]};
    width = Max[indices];
    height = qco["Arity"];
    sizes = Most @ Rasterize[#, "BoundingBox"] & /@ labels;
    imageWidth = Min[width Max[sizes[[All, 1]] + 1, 64] scale, 512];
    imageHeight = Min[height Max[sizes[[All, 2]] + 1, 64] scale, 512];
    Show[graphics,
        FilterRules[{opts}, Options[Graphics]],
        ImageSize -> If[width > height, {imageWidth, Automatic}, {Automatic, imageHeight}]
    ]
]

QuantumCircuitOperatorProp[qco_, "CircuitOperator" | "Compile"] := Fold[ReverseApplied[Construct], qco["Operators"]]

QuantumCircuitOperatorProp[qco_, "Gates"] := Length @ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "InputOrders"] := #["InputOrder"] & /@ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "InputOrder"] := Union @@ qco["InputOrders"]

QuantumCircuitOperatorProp[qco_, "OutputOrders"] := #["OutputOrder"] & /@ qco["Operators"]

QuantumCircuitOperatorProp[qco_, "OutputOrder"] := Union @@ qco["OutputOrders"]

QuantumCircuitOperatorProp[qco_, "Arity"] := Length @ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimensions"] :=
    (q |-> #["InputDimensions"][[ q /. #["InputOrderQuditMapping"] ]] & @
        SelectFirst[qco["Operators"], op |-> MemberQ[op["FullInputOrder"], q]]) /@ qco["InputOrder"]

QuantumCircuitOperatorProp[qco_, "InputDimension"] := Times @@ qco["InputDimensions"]

QuantumCircuitOperatorProp[qco_, "OutputDimensions"] :=
    (q |-> #["OutputDimensions"][[ q /. #["OutputOrderQuditMapping"] ]] & @
        SelectFirst[Reverse @ qco["Operators"], op |-> MemberQ[op["FullOutputOrder"], q]]) /@ qco["OutputOrder"]

QuantumCircuitOperatorProp[qco_, "OutputDimension"] := Times @@ qco["OutputDimensions"]

QuantumCircuitOperatorProp[qco_, "Target"] := Union @@ (#["Target"] & /@ Select[qco["Operators"], QuantumMeasurementOperatorQ])

QuantumCircuitOperatorProp[qco_, "QiskitCircuit" | "Qiskit"] := QuantumCircuitOperatorToQiskit[qco]

(* operator properties *)

QuantumCircuitOperatorProp[qco_, args : PatternSequence[prop_String, ___] | PatternSequence[{prop_String, ___}, ___]] /;
    MemberQ[Intersection[Last[qco["Operators"]]["Properties"], qco["Properties"]], prop] := qco["CircuitOperator"][args]

