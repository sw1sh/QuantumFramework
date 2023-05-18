Package["Wolfram`QuantumFramework`"]

PackageExport["QiskitCircuit"]
PackageExport["ImportQASMCircuit"]

PackageScope["QuantumCircuitOperatorToQiskit"]



existingGateNames = {
    "barrier", "measure", "reset", "u3", "u2", "u1",
    "cx", "id", "u0", "u", "p", "x", "y", "z", "h", "s", "sdg", "t",
    "tdg", "rx", "ry", "rz", "sx", "sxdg", "cz", "cy", "swap", "ch",
    "ccx", "cswap", "crx", "cry", "crz", "cu1", "cp", "cu3", "csx",
    "cu", "rxx", "rzz", "rccx", "rc3x", "c3x", "c3sx", "c4x"
};

labelToGate = Replace[{
    Subscript["C", subLabel_][{}, {}] :> labelToGate[subLabel],
    Subscript["C", x_String][__] :> "c" <> Replace[ToLowerCase[x], "not" -> "x"],
    Subscript["C", x_String[args__]][__] :> {"c" <> ToLowerCase[x], Sequence @@ N[{args}]},
    SuperDagger[x_String] :> ToLowerCase[x] <> "dg",
    Subscript["R", axis_String][angle_] :> {"r" <> ToLowerCase[axis], N @ angle},
    Subscript["C", Subscript["R", axis_String][angle_]][__] :> {"cr" <> ToLowerCase[axis], N @ angle},
    Subscript["C", Superscript[Subscript["R", axis_String][angle_], CircleTimes[range_]]][__] :> {"cr" <> ToLowerCase[axis], N @ angle, range},
    "\[Pi]"[perm___] :> {"perm", PermutationCycles[{perm}]},
    x_String :> ToLowerCase[x],
    x_String[params___] :> {ToLowerCase[x], params}
}]

QuantumCircuitOperatorToQiskit[qco_QuantumCircuitOperator] := Enclose @ Block[{
    operators = Map[
        With[{
            label = Which[BarrierQ[#], {"barrier", #}, QuantumMeasurementOperatorQ[#], "m", True, labelToGate @ #["Label"]],
            nTargets = qco["Targets"],
            targetIndex = First /@ PositionIndex[qco["Target"]]
        },
            Replace[
                label, {
                {"barrier", barrier_} :> With[{order = circuitElementOrder[barrier, qco["Width"]]}, {
                    "barrier",
                    {},
                    order - 1
                }],
                "m" :>
                    Splice[With[{target = #["Target"]}, Map[{label, None, {# - 1, nTargets - targetIndex[#]}} &, target]]],
                (name : "cx" | "cy" | "cz" | "ch" | "cswap") | {name : "crx" | "cry" | "crz" | "cp" | "cu2" | "cu", params___} :>
                    Block[{c1, c0, t = #["TargetOrder"], range},
                        {c1, c0} = Replace[#["Label"], Subscript["C", _][c1_, c0_] :> {c1, c0}];
                        range = Join[c1, c0];
                        {
                            "control",
                            {
                                Length[c1] + Length[c0],
                                name,
                                StringReverse @ StringJoin @ Replace[range, Join[Thread[c1 -> "1"], Thread[c0 -> "0"]], {1}],
                                {params}
                            },
                            Join[range, t] - 1
                        }
                    ],
                {"perm", Cycles[{swap : {_, _}}]} :> {"swap", {}, swap - 1},
                {"perm", perm_Cycles} :> {"permutation", PermutationList[perm] - 1, #["InputOrder"] - 1},
                {name_String /; MemberQ[existingGateNames, name], params___} :> {
                    name,
                    N @ {params},
                    #["InputOrder"] - 1
                },
                name_String /; MemberQ[existingGateNames, name] :> {
                    name,
                    {},
                    #["InputOrder"] - 1
                },
                _ :> {
                    ToString[label],
                    NumericArray @ N @ Normal @ #["Sort"]["Reverse"]["MatrixRepresentation"],
                    #["InputOrder"] - 1
                }
            }
            ]
        ] &,
        QuantumCircuitOperator[QuantumShortcut[qco]]["Flatten"]["Elements"]
    ],
    arity = {qco["Width"], Replace[Quiet[qco["TargetArity"]], Except[_Integer] -> Nothing]}
},
    Confirm @ PythonEvaluate[Context[arity], "
from wolframclient.language import wl

from qiskit import QuantumCircuit
from qiskit.extensions import UnitaryGate
from qiskit.circuit.gate import Gate
from qiskit.circuit.library import XGate, YGate, ZGate, HGate, RXGate, RYGate, RZGate, PhaseGate, U2Gate, U3Gate, SwapGate, PermutationGate

import pickle

circuit = QuantumCircuit(
    * <* arity *>
)

for name, data, order in <* operators *>:
    if name.lower() in <* existingGateNames *>:
        getattr(circuit, name.lower())(*data, *tuple(order))
    elif name == 'control':
        base_name = data[1][1:]
        if base_name == 'x':
            base_gate = XGate()
        elif base_name == 'y':
            base_gate = YGate()
        elif base_name == 'z':
            base_gate = ZGate()
        elif base_name == 'h':
            base_gate = HGate()
        elif base_name == 'rx':
            base_gate = RXGate(*data[3])
        elif base_name == 'ry':
            base_gate = RYGate(*data[3])
        elif base_name == 'rz':
            base_gate = RZGate(*data[3])
        elif base_name == 'p':
            base_gate = PhaseGate(*data[3])
        elif base_name == 'u2':
            base_gate = U2Gate(*data[3])
        elif base_name == 'u':
            base_gate = U3Gate(*data[3])
        elif base_name == 'swap':
            base_gate = SwapGate(*data[3])
        else:
            base_gate = Gate(base_name, n_qubits=data[0])
        circuit.append(base_gate.control(len(data[2]), ctrl_state=data[2]), tuple(order))
    elif name == 'permutation':
        circuit.append(PermutationGate(data), tuple(order))
    elif name == 'm':
        circuit.measure(*tuple(order))
    else:
        circuit.append(UnitaryGate(data, name), tuple(sorted(order)))
wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(circuit))
    "]
]

QiskitCircuit[bytes_ByteArray]["Bytes"] := bytes

qc_QiskitCircuit["Eval", attr_String, args___, kwargs : OptionsPattern[]] :=
    PythonEvalAttr[{qc["Bytes"], attr, args, kwargs}]

qc_QiskitCircuit["EvalBytes", attr_String, args___, kwargs : OptionsPattern[]] :=
    PythonEvalAttr[{qc["Bytes"], attr, args, kwargs}, "ReturnBytes" -> True]


qiskitGraphics[qc_QiskitCircuit, OptionsPattern[{"Scale" -> 5}]] := Enclose @ With[{
    latex = (
        PythonEvaluate["
import os
if 'texbin' not in os.environ['PATH']:
    os.environ['PATH'] += os.pathsep + os.pathsep.join(['/Library/TeX/texbin'])
"];
        qc["Eval", "draw", "output" -> "latex_source", "scale" -> OptionValue["Scale"]]
    )
},
    Confirm @ Check[Needs["MaTeX`"], ResourceFunction["MaTeXInstall"][]; Needs["MaTeX`"]];
    MaTeX`MaTeX[
        First @ StringCases[latex, "\\begin{document}" ~~ s___ ~~ "\\end{document}" :> s],
        "Preamble" -> Prepend[StringCases[latex, s : ("\\usepackage" ~~ Shortest[___] ~~ EndOfLine) :> s], "\\standaloneconfig{border=-16 -8 -16 -16}"]
    ]
]

Options[qiskitDiagram] = {"Scale" -> 5}

qiskitDiagram[qc_QiskitCircuit, opts : OptionsPattern[]] := Enclose @ Block[{
    $pythonBytes = qc["Bytes"],
    scale = OptionValue["Scale"],
    image
},
    image = PythonEvaluate[Context[scale], "
import pickle
import PIL
import matplotlib
matplotlib.use('Agg')
fig = pickle.loads(<* $pythonBytes *>).draw(output='mpl', scale=<* scale *>)
fig.canvas.draw()
PIL.Image.frombytes('RGB', fig.canvas.get_width_height(), fig.canvas.tostring_rgb())
"];
    ImageCrop[ConfirmBy[image, ImageQ]]
]


qc_QiskitCircuit["Graphics", opts : OptionsPattern[]] := qiskitGraphics[qc, opts]

qc_QiskitCircuit["Diagram", opts : OptionsPattern[]] := qiskitDiagram[qc, opts]

qc_QiskitCircuit["Qubits"] := qc["Eval", "num_qubits"]

qc_QiskitCircuit["Depth"] := qc["Eval", "depth"]

qc_QiskitCircuit["Ops"] := qc["Eval", "count_ops"]

qc_QiskitCircuit["Clbits"] := Block[{
    $pythonBytes = qc["Bytes"]
},
    PythonEvaluate[Context[$pythonBytes], "
import pickle
qc = pickle.loads(<* $pythonBytes *>)
[clbit.index for clbit in qc.clbits]
"]
]

qc_QiskitCircuit["QuantumCircuit" | "QuantumCircuitOperator"] := Block[{
    $pythonBytes = qc["Bytes"]
},
    PythonEvaluate[Context[$pythonBytes], "
import pickle
from wolframclient.language import wl
from math import pi
from qiskit.circuit import ParameterExpression

qc = pickle.loads(<* $pythonBytes *>)

ops = []
for gate, qubits, clbits in qc:
    order = []
    for q in qubits:
        size = 0
        for r in qc.qubits:
            if r.register.name == q.register.name:
                order.append(size + q.index + 1)
                break
            else:
                size = r.index + 1
    if len(gate.params) > 0:
        xs = []
        for x in gate.params:
            if isinstance(x, float):
                xs.append(wl.Wolfram.QuantumFramework.PackageScope.TranscendentalRecognize(x))
            elif isinstance(x, ParameterExpression):
                xs.append(wl.ToExpression(str(x)))
            else:
                xs.append(x)
    if gate.name == 'x':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('X', order))
    elif gate.name == 'y':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('Y', order))
    elif gate.name == 'z':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('Z', order))
    elif gate.name == 'h':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('H', order))
    elif gate.name == 'p':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Phase', xs[0]], order))
    elif gate.name == 'u':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['U', *xs], order))
    elif gate.name == 'u1':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['U1', *xs], order))
    elif gate.name == 'u2':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['U2', *xs], order))
    elif gate.name == 'u3':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['U3', *xs], order))
    elif gate.name == 'rx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['RX', *xs], order))
    elif gate.name == 'ry':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['RY', *xs], order))
    elif gate.name == 'rz':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['RZ', *xs], order))
    elif gate.name == 'cx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CNOT', order))
    elif gate.name == 'cy':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CY', order))
    elif gate.name == 'cz':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CZ', order))
    elif gate.name == 'crx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C', ['R', *xs, 'X']], order))
    elif gate.name == 'cry':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C', ['R', *xs, 'Y']], order))
    elif gate.name == 'crz':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C', ['R', *xs, 'Z']], order))
    elif gate.name == 'c    rx_o0':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C0', ['R', *xs, 'X']], order))
    elif gate.name == 'cry_o0':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C0', ['R', *xs, 'Y']], order))
    elif gate.name == 'crz_o0':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['C0', ['R', *xs, 'Z']], order))
    elif gate.name == 't':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('T', order))
    elif gate.name == 'tdg':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('T', order)('Dagger'))
    elif gate.name == 's':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('S', order))
    elif gate.name == 'sdg':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('S', order)('Dagger'))
    elif gate.name == 'ccrx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', ['XRotation', *xs], order[:2]], order[2:]))
    elif gate.name == 'ccrx_o0':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled0', ['XRotation', *xs], order[:2]], order[2:]))
    elif gate.name == 'ccrx_o1':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', ['XRotation', *xs], [order[0]], [order[1]]], order[2:]))
    elif gate.name == 'ccrx_o2':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', ['XRotation', *xs], [order[1]], [order[0]]], order[2:]))
    elif gate.name == 'ccx_o2':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', 'NOT', [order[1]], [order[0]]], order[2:]))
    elif gate.name == 'ccx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', 'NOT', order[:2]], order[2:]))
    elif gate.name == 'mcx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Controlled', 'NOT', order[:-1]], order[-1:]))
    elif gate.name == 'swap':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('SWAP', order))
    elif gate.name == 'unitary':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(*xs, wl.Rule('Label', gate.label)))
    elif gate.name == 'measure':
        ops.append(wl.Wolfram.QuantumFramework.QuantumMeasurementOperator(order))
    elif gate.name == 'barrier':
        ops.append('Barrier')
    else:
        print('Unknonwn gate: ', gate.name, gate.params, [q.index for q in qubits])
wl.Wolfram.QuantumFramework.QuantumCircuitOperator(ops)
"]
]


qiskitMatrix[qc_QiskitCircuit] := Block[{$pythonBytes = qc["Bytes"]},
    PythonEvaluate[Context[$pythonBytes], "
from qiskit import BasicAer, transpile

import pickle

qc = pickle.loads(<* $pythonBytes *>)

backend = BasicAer.get_backend('unitary_simulator')

job = backend.run(transpile(qc, backend))
job.result().get_unitary(qc, decimals=3)
"]
]

Options[qiskitInitBackend] = {"Provider" -> None, "Backend" -> Automatic}

qiskitInitBackend[qc_QiskitCircuit, OptionsPattern[]] := Enclose @ Block[{
    $pythonBytes = qc["Bytes"],
    provider,
    $backendName = Replace[OptionValue["Backend"], Automatic -> Null],
    $token = Null
},
    {provider, params} = Replace[OptionValue["Provider"], {
        {name_, params : OptionsPattern[]} | name_ :> {name, Flatten[{params}]}
    }];

    If[ MatchQ[provider, "IBMQ" | "IBMProvider"],
        $token = Lookup[params, "Token", Null];
    ];

    Confirm @ PythonEvaluate[Context[$pythonBytes], Switch[provider,
    "IBMQ",
"
try:
    assert(isinstance(provider, AccountProvider))
except:
    from qiskit import IBMQ
    from qiskit.providers.ibmq.accountprovider import AccountProvider
    token = <* $token *>
    if token is not None:
        from qiskit_ibm_runtime import QiskitRuntimeService
        QiskitRuntimeService.save_account(channel='ibm_quantum', token=token, overwrite=True)
    provider = IBMQ.load_account()
",
    "IBMProvider",
"
try:
    assert(isinstance(provider, IBMProvider))
except:
    from qiskit_ibm_provider import IBMProvider
    token = <* $token *>
    if token is not None:
        IBMProvider.save_account(token=token, overwrite=True)
    provider = IBMProvider()
",
    "AWSBraket",
"
from qiskit_braket_provider import AWSBraketProvider

try:
    assert(isinstance(provider, AWSBraketProvider))
except:
    from qiskit_braket_provider import AWSBraketProvider
    provider = AWSBraketProvider()
",
    _,
"
provider = None
"]
    ];
    PythonEvaluate[Context[$pythonBytes], "
import pickle
from qiskit import Aer
from qiskit_braket_provider import AWSBraketProvider

qc = pickle.loads(<* $pythonBytes *>)
backend_name = <* $backendName *>
if provider is None:
    if qc.num_clbits > 0:
        backend = Aer.get_backend('qasm_simulator')
    else:
        backend = Aer.get_backend('statevector_simulator')
else:
    if backend_name is None:
        if isinstance(provider, AWSBraketProvider):
            backend = provider.get_backend('SV1')
        else:
            if qc.num_clbits > 0:
                backend = provider.get_backend('ibmq_qasm_simulator')
            else:
                backend = provider.get_backend('simulator_statevector')
    else:
        backend = provider.get_backend(backend_name)
"
     ]
]

Options[qiskitApply] = Join[{"Shots" -> 1024}, Options[qiskitInitBackend]]

qiskitApply[qc_QiskitCircuit, qs_QuantumState, opts : OptionsPattern[]] := Enclose @ Block[{
    $state = NumericArray @ N @ qs["Reverse"]["StateVector"],
    $shots = OptionValue["Shots"],
    result
},
    ConfirmAssert[qs["InputDimensions"] == {}];
    ConfirmAssert[AllTrue[qs["OutputDimensions"], EqualTo[2]]];

    Confirm @ qiskitInitBackend[qc, FilterRules[{opts}, Options[qiskitInitBackend]]];

    result = PythonEvaluate[Context[$state], "
import pickle
from qiskit import QuantumCircuit
from qiskit import transpile
from qiskit_braket_provider import AWSBraketProvider

# Run the quantum circuit on a statevector simulator backend

circuit = QuantumCircuit(qc.num_qubits, qc.num_clbits)


if not isinstance(provider, AWSBraketProvider):
    circuit.initialize(<* $state *>)
circuit = circuit.compose(qc)

circuit = transpile(circuit, backend)

result = backend.run(circuit, shots = <* $shots *>).result()

if isinstance(provider, AWSBraketProvider):
    result = result.get_counts()
else:
    if qc.num_clbits > 0:
        result = result.get_counts(circuit)
    else:
        result = result.get_statevector().data
result
"];
    Which[
        AssociationQ[result],
        Enclose[
            With[{size = StringLength @ First @ Keys[result]},
                ConfirmAssert[size <= 8];
                ConfirmBy[
                    QuantumMeasurement[
                        Join[
                            Association[# -> 0 & /@ IntegerDigits[Range[2 ^ size] - 1, 2, size]],
                            KeyMap[Characters[#] /. {"0" -> 0, "1" -> 1} &] @ result
                        ]
                    ],
                    QuantumMeasurementQ
                ]
            ],
            result &
        ],
        NumericArrayQ[result],
        QuantumState[Chop @ Normal[result]]["Reverse"],
        True,
        result
    ]
]

qc_QiskitCircuit["Decompose", n : _Integer ? Positive : 1] := Nest[QiskitCircuit[#["EvalBytes", "decompose"]] &, qc, n]

qc_QiskitCircuit["QuantumOperator"] := Enclose @ Module[{mat = Chop @ Normal[ConfirmBy[qiskitMatrix[qc], NumericArrayQ]]},
    ConfirmAssert[SquareMatrixQ[mat]];
    QuantumOperator[mat]["Reverse"]["Sort"]
]

qc_QiskitCircuit["Matrix"] := qc["QuantumOperator"]["Matrix"]

qc_QiskitCircuit[opts : OptionsPattern[qiskitApply]] := qiskitApply[qc, QuantumState[{"Register", qc["Qubits"]}], opts]

qc_QiskitCircuit[qs_QuantumState, opts : OptionsPattern[qiskitApply]] := qiskitApply[qc, qs, opts]

qc_QiskitCircuit["QASM", opts : OptionsPattern[qiskitInitBackend]] := Enclose[
    Confirm @ qiskitInitBackend[qc, opts];
    PythonEvaluate["
from qiskit import transpile
from qiskit_braket_provider import AWSBraketProvider

circuit = transpile(qc, backend)
if isinstance(provider, AWSBraketProvider):
    from qiskit_braket_provider.providers.adapter import convert_qiskit_to_braket_circuit
    from braket.circuits.serialization import IRType
    result = convert_qiskit_to_braket_circuit(circuit).to_ir(IRType.OPENQASM).source
else:
    result = circuit.qasm()
result
"]
]


qc_QiskitCircuit["QPY", opts : OptionsPattern[qiskitInitBackend]] := Enclose[
    Confirm @ qiskitInitBackend[qc, opts];
    PythonEvaluate["
from qiskit import qpy
from io import BytesIO
from qiskit import transpile
import zlib

qc = transpile(qc, backend)
bytes = BytesIO()
qpy.dump(qc, bytes)
zlib.compress(bytes.getvalue())
"]
]

qc_QiskitCircuit["Transpile", opts : OptionsPattern[qiskitInitBackend]]:= Enclose[
    Confirm @ qiskitInitBackend[qc, opts];
    PythonEvaluate["
import pickle
from qiskit import transpile
from wolframclient.language import wl

wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(transpile(qc, backend)))
"]
]


ImportQASMCircuit[file_ /; FileExistsQ[file], basisGates : {_String...} | None] := ImportQASMCircuit[Import[file, "String"], basisGates]

ImportQASMCircuit[str_String, backend : _String | Automatic : Automatic, basisGates : {_String...} | None : None] := Block[{
    $pythonString = str,
    $backend = Replace[backend, Automatic -> Null],
    $basisGates = Replace[basisGates, None -> Null]
},
PythonEvaluate[Context[$backend], "
import pickle
from qiskit import QuantumCircuit, transpile
from wolframclient.language import wl

qc = QuantumCircuit.from_qasm_str(<* $pythonString *>)

qc = transpile(qc, backend=<* $backend *>, basis_gates=<* $basisGates *>)
wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(qc))
"
]
]


(* Formatting *)

QiskitCircuit /: MakeBoxes[qc_QiskitCircuit, format_] :=
    BoxForm`ArrangeSummaryBox[
        "QiskitCircuit",
        qc,
        qc["Diagram", "Scale" -> 1],
        {
            {BoxForm`SummaryItem[{"Qubits: ", qc["Qubits"]}]},
            {BoxForm`SummaryItem[{"Depth: ", qc["Depth"]}]}
        },
        {
            {BoxForm`SummaryItem[{"Ops: ", qc["Ops"]}]}
        },
        format,
        "Interpretable" -> Automatic
    ]

