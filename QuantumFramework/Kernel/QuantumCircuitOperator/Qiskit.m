Package["Wolfram`QuantumFramework`"]

PackageExport["QiskitCircuit"]
PackageExport["ImportQASMCircuit"]
PackageExport["ImportQPY"]

PackageScope["QuantumCircuitOperatorToQiskit"]



shortcutToGate = Replace[
    {
        {"R", angle_, {name_ -> order_ ? orderQ}} :> shortcutToGate[{"R", angle, name}] -> order,
        (name_ -> (order_ ? orderQ)) :> shortcutToGate[name] -> order,
        "X" | "NOT" -> "XGate",
        "Y" -> "YGate",
        "Z" | "1" -> "ZGate",
        "H" -> "HGate",
        "S" -> "SGate",
        "T" -> "TGate",
        "V" -> "SXGate",
        "SWAP" -> "SwapGate",
        {"Diagonal", x_} :> If[ListQ[x], {"Diagonal", NumericArray @ N[x]}, {"GlobalPhaseGate", N[Arg[x]]}],
        {"U2", a_, b_} :> {"U2Gate", N[a], N[b]},
        {"U", a_, b_, c_} :> {"U3Gate", N[a], N[b], N[c]},
        {"Permutation", perm_} :> {"PermutationGate", PermutationList[perm] - 1},
        {"GlobalPhase", phase_} :> {"GlobalPhaseGate", N[phase]},
        {"R", angle_, "X"} :> {"RXGate", N[angle]},
        {"R", angle_, "Y"} :> {"RYGate", N[angle]},
        {"R", angle_, "Z"} :> {"RZGate", N[angle]},
        {"P", phase_} :> {"PhaseGate", N[phase]},
        {"PhaseShift", k_} :> {"PhaseGate", N[Sign[k] 2 Pi / 2 ^ Abs[k]]},
        {"C", name_, controls___} :> {"Control", shortcutToGate[name], controls},
        SuperDagger[name_] :> {"Dagger", shortcutToGate[name]},
        barrier: "Barrier" | "Barrier"[arg_] :> "Barrier",
        Labeled[arr_ /; ArrayQ[arr] || NumericArrayQ[arr] -> order_, label_] :> {"Unitary", NumericArray[Normal @ N @ arr, "ComplexReal32"], ToString[label], order},
        target_ ? orderQ :> {"Measure", target},
        shortcut_ :> With[{op = QuantumOperator[shortcut]}, {"Unitary", NumericArray[Normal @ N @ op["Matrix"], "ComplexReal32"], ToString[shortcut], op["Order"]}]
    }
]

QuantumCircuitOperatorToQiskit[qco_QuantumCircuitOperator] := Enclose @ Block[{
    gates = shortcutToGate /@ Catenate[QuantumShortcut /@ qco["Flatten"]["Elements"]],
    arity = {qco["Max"], Replace[Quiet[qco["TargetArity"]], Except[_Integer] -> Nothing]}
},
    Confirm @ PythonEvaluate[Context[arity], "
from wolframclient.language import wl
from wolframclient.language.expression import WLFunction

from qiskit import QuantumCircuit
from qiskit.extensions import UnitaryGate
from qiskit.circuit.gate import Gate
from qiskit.circuit.library import *

import pickle

circuit = QuantumCircuit(
    * <* arity *>
)

def make_gate(gate_spec):
    order = target = []
    if type(gate_spec) == WLFunction and gate_spec.head == wl.Rule:
        gate, order, target = make_gate(gate_spec[0])
        order = list(gate_spec[1])
        return gate, order, target
    elif isinstance(gate_spec, tuple):
        name, *args = gate_spec
    else:
        name = gate_spec
        args = []
    if name == 'Control':
        base_gate, base_order, base_target = make_gate(args[0])
        size1 = len(args[1])
        size0 = len(args[2])
        gate = base_gate.control(size1 + size0, ctrl_state='0' * size0 + '1' * size1)
        order = list(args[1]) + list(args[2])
        if base_gate.name != 'global_phase':
            order = order + base_order
    elif name == 'Dagger':
        base_gate = make_gate(args[0])[0]
        gate = base_gate.inverse()
    elif name == 'Unitary':
        gate = UnitaryGate(args[0], label=args[1])
        assert(args[2][0] == args[2][1])
        order = list(args[2][0])
    elif name == 'Barrier':
        gate = Barrier(len(order))
    elif name == 'Measure':
        target = order = list(args[0])
        gate = Measure()
    else:
        gate = getattr(qiskit.circuit.library, name)(*args)
    return gate, order, target

def add_gate(circuit, gate_spec, c):
    gate, order, target = make_gate(gate_spec)
    for q in range(len(order)):
        order[q] -= 1
    if gate.name == 'global_phase':
        order = []
    if len(target) > 0:
        for i, t in enumerate(target):
            circuit.append(gate, [t], (i + c, ))
    else:
        circuit.append(gate, tuple(order))
    return c + len(target)

c = 0

for gate_spec in <* gates *>:
    c = add_gate(circuit, gate_spec, c)

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
image = PIL.Image.frombytes('RGB', fig.canvas.get_width_height(), fig.canvas.tostring_rgb())
matplotlib.pyplot.close()
image
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

def reverse_binary(n, width):
    b = '{:0{width}b}'.format(n, width=width)
    return int(b[::-1], 2)

def gate_to_QuantumOperator(gate, order):
    xs = []
    if len(gate.params) > 0:
        for x in gate.params:
            if isinstance(x, float):
                xs.append(wl.Wolfram.QuantumFramework.PackageScope.TranscendentalRecognize(x))
            elif isinstance(x, ParameterExpression):
                xs.append(wl.ToExpression(str(x)))
            else:
                xs.append(x)
    if gate.name == 'x':
        return wl.Wolfram.QuantumFramework.QuantumOperator('X', order)
    elif gate.name == 'y':
        return wl.Wolfram.QuantumFramework.QuantumOperator('Y', order)
    elif gate.name == 'z':
        return wl.Wolfram.QuantumFramework.QuantumOperator('Z', order)
    elif gate.name == 'h':
        return wl.Wolfram.QuantumFramework.QuantumOperator('H', order)
    elif gate.name == 'p':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['Phase', xs[0]], order)
    elif gate.name == 'u':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['U', *xs], order)
    elif gate.name == 'u1':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['U1', *xs], order)
    elif gate.name == 'u2':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['U2', *xs], order)
    elif gate.name == 'u3':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['U3', *xs], order)
    elif gate.name == 'rx':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['RX', *xs], order)
    elif gate.name == 'ry':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['RY', *xs], order)
    elif gate.name == 'rz':
        return wl.Wolfram.QuantumFramework.QuantumOperator(['RZ', *xs], order)
    elif gate.name == 't':
        return wl.Wolfram.QuantumFramework.QuantumOperator('T', order)
    elif gate.name == 'tdg':
        return wl.Wolfram.QuantumFramework.QuantumOperator('T', order)('Dagger')
    elif gate.name == 's':
        return wl.Wolfram.QuantumFramework.QuantumOperator('S', order)
    elif gate.name == 'sdg':
        return wl.Wolfram.QuantumFramework.QuantumOperator('S', order)('Dagger')
    elif gate.name == 'swap':
        return wl.Wolfram.QuantumFramework.QuantumOperator('SWAP', order)
    elif gate.name == 'unitary':
        return wl.Wolfram.QuantumFramework.QuantumOperator(*xs, [order, order], wl.Rule('Label', gate.label))
    elif gate.name == 'measure':
        return wl.Wolfram.QuantumFramework.QuantumMeasurementOperator(order)
    elif gate.name == 'reset':
        ops.extend([
            wl.Wolfram.QuantumFramework.QuantumOperator('Discard', order),
            wl.Wolfram.QuantumFramework.QuantumOperator(wl.Wolfram.QuantumFramework.QuantumState(('Register', len(order)), wl.Rule('Label', wl.Ket(0))), order)
        ])
    elif gate.name == 'barrier':
        return 'Barrier'
    elif hasattr(gate, 'num_ctrl_qubits'):
        arg = ['C', gate_to_QuantumOperator(gate.base_gate, order[gate.num_ctrl_qubits:]), reverse_binary(gate.ctrl_state, gate.num_ctrl_qubits), order[:gate.num_ctrl_qubits]]
        return wl.Wolfram.QuantumFramework.QuantumOperator(arg)
    else:
        from qiskit.quantum_info import Operator
        return wl.Wolfram.QuantumFramework.QuantumOperator(Operator(gate).to_matrix(), [order, order], wl.Rule('Label', gate.label))

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
    ops.append(gate_to_QuantumOperator(gate, order))

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
    provider, params,
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
        backend = Aer.get_backend('qasm_simulator' if backend_name is None else backend_name)
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

Options[qiskitApply] = Join[{"Shots" -> 1024, "FireOpal" -> False}, Options[qiskitInitBackend]]

qiskitApply[qc_QiskitCircuit, qs_QuantumState, opts : OptionsPattern[]] := Enclose @ Block[{
    $state = NumericArray @ N @ qs["Reverse"]["StateVector"],
    $shots = OptionValue["Shots"],
    $fireOpal = TrueQ[OptionValue["FireOpal"]],
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

try:
    circuit = transpile(circuit, backend)
except:
    pass

if <* $fireOpal *>:
    import fireopal
    qasm = circuit.qasm()
    credentials = {'token': provider._account.token, 'group': 'open', 'hub': 'ibm-q', 'project': 'main', 'provider': 'ibmq'}
    validate_results = fireopal.validate(
        circuits=[qasm], credentials=credentials, backend_name=backend.name
    )
    assert(validate_results['results'] == [])
    result = fireopal.execute(
        circuits=[qasm],
        shot_count=<* $shots *>,
        credentials=credentials,
        backend_name=backend.name,
    )['results'][0]
    result = {k[::-1]: v for k, v in result.items()}
else:
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
            Block[{counts = KeyMap[StringDelete[Whitespace]] @ result, size},
                size = StringLength @ First @ Keys[counts];
                ConfirmAssert[size <= 8];
                ConfirmBy[
                    QuantumMeasurement[
                        Join[
                            Association[# -> 0 & /@ IntegerDigits[Range[2 ^ size] - 1, 2, size]],
                            KeyMap[Characters[#] /. {"0" -> 0, "1" -> 1} &] @ counts
                        ]
                    ],
                    QuantumMeasurementQ
                ]
            ],
            counts &
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

ImportQPY[qpy_ByteArray] := Block[{$qpy = qpy}, PythonEvaluate[Context[$qpy], "
from qiskit import qpy
import zlib
from io import BytesIO
from wolframclient.language import wl
import pickle

qcs = qpy.load(BytesIO(zlib.decompress(<* $qpy *>)))
[wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(qc)) for qc in qcs]
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

