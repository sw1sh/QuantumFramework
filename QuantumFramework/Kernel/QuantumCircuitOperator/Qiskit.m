Package["Wolfram`QuantumFramework`"]

PackageExport["QiskitCircuit"]
PackageExport["ImportQASMCircuit"]
PackageExport["ImportQPY"]

PackageScope["QuantumCircuitOperatorToQiskit"]



shortcutToGate = Replace[
    {
        {"R", angle_, {name_ -> order_ ? orderQ}} :> shortcutToGate[{"R", angle, name}] -> order,
        ("M" -> target_ ? orderQ) :> {"Measure", target},
        ({"M", args___} -> target_ ? orderQ) :> Splice @ Append[shortcutToGate /@ QuantumShortcut[QuantumOperator[Inverse[QuantumBasis[args]["Matrix"]]]], {"Measure", target}],
        (name_ -> (order_ ? orderQ)) :> With[{shortcut = shortcutToGate[name]}, If[shortcut === Nothing, Nothing, shortcut -> order]],
        "I" -> Nothing,
        "X" | "NOT" -> "XGate",
        "Y" -> "YGate",
        "Z" | "1" -> "ZGate",
        "H" -> "HGate",
        "S" -> "SGate",
        "T" -> "TGate",
        "V" -> "SXGate",
        "SWAP" -> "SwapGate",
        {"Diagonal", x_} :> If[ListQ[x], {"Diagonal", NumericArray @ N[x]}, {"GlobalPhaseGate", Chop[N[Arg[x]]]}],
        {"U2", a_, b_} :> {"U2Gate", N[a], N[b]},
        {"U", a_, b_, c_} :> {"U3Gate", N[a], N[b], N[c]},
        {"Permutation", perm_} :> {"PermutationGate", PermutationList[perm] - 1},
        {"GlobalPhase", phase_} :> {"GlobalPhaseGate", Chop[N[phase]]},
        {"R", angle_, "X"} :> {"RXGate", N[angle]},
        {"R", angle_, "Y"} :> {"RYGate", N[angle]},
        {"R", angle_, "Z"} :> {"RZGate", N[angle]},
        {"P", phase_} :> {"PhaseGate", N[phase]},
        {"PhaseShift", k_} :> {"PhaseGate", N[Sign[k] 2 Pi / 2 ^ Abs[k]]},
        {"C", name_, controls___} :> {"Control", shortcutToGate[name], controls},
        SuperDagger[name_] :> {"Dagger", shortcutToGate[name]},
        barrier: "Barrier" | "Barrier"[___] :> "Barrier",
        Labeled[arr_ /; ArrayQ[arr] || NumericArrayQ[arr] -> order_, label_] :> If[
            order[[2]] === {},
            With[{state = QuantumOperator[arr, order]["State"]},
                If[ MatchQ[order[[1]], {_}] && state == QuantumState[1],
                    Nothing,
                    Splice[shortcutToGate /@ Catenate[QuantumShortcut /@ QuantumCircuitOperator[state, order[[1]]]["Operators"][[state["OutputQudits"] + 1 ;;]]]]
                ]
            ],
            {"Unitary", NumericArray[Normal @ N @ arr, "ComplexReal32"], ToString[label], order}
        ],
        shortcut_ :> With[{op = QuantumOperator[shortcut]}, {"Unitary", NumericArray[Normal @ N @ op["Matrix"], "ComplexReal32"], ToString[shortcut], op["Order"]}]
    }
]

QuantumCircuitOperatorToQiskit[qco_QuantumCircuitOperator] := Enclose @ Block[{
    gates = Confirm @* shortcutToGate /@ Catenate[QuantumShortcut /@ qco["Flatten"]["Elements"]],
    arity = {qco["Max"], Replace[Quiet[qco["TargetArity"]], Except[_Integer] -> Nothing]}
},
    Confirm @ PythonEvaluate[Context[arity], "
from wolframclient.language import wl
from wolframclient.language.expression import WLFunction

from qiskit import QuantumCircuit
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
        gate = UnitaryGate(args[0], label=args[1], check_input=False)
        assert(all(i == j for i, j in zip(args[2][0], args[2][1])))
        order = list(args[2][0])[::-1]
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

QiskitCircuit[qc_QuantumCircuitOperator] := qc["Qiskit"]

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
    image = Confirm @ PythonEvaluate[Context[scale], "
import pickle
import PIL
import matplotlib
matplotlib.use('Agg')
fig = pickle.loads(<* $pythonBytes *>).draw(output='mpl', style='iqp', scale=<* scale *>)
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
        return wl.Wolfram.QuantumFramework.QuantumOperator('NOT', order)
    elif gate.name in ['y', 'z', 'h', 't', 's', 'swap']:
        return wl.Wolfram.QuantumFramework.QuantumOperator(gate.name.upper(), order)
    elif gate.name in ['p', 'u', 'u1', 'u2', 'u3', 'rx', 'ry', 'rz', 'sx']:
        return wl.Wolfram.QuantumFramework.QuantumOperator([gate.name.upper(), *xs], order)
    elif gate.name == 'tdg':
        return wl.Wolfram.QuantumFramework.QuantumOperator('T', order)('Dagger')
    elif gate.name == 'sdg':
        return wl.Wolfram.QuantumFramework.QuantumOperator('S', order)('Dagger')
    elif gate.name == 'unitary':
        return wl.Wolfram.QuantumFramework.QuantumOperator(*xs, [order, order], wl.Rule('Label', gate.name if gate.name else None))
    elif gate.name == 'measure':
        return wl.Wolfram.QuantumFramework.QuantumMeasurementOperator(order)
    elif gate.name == 'reset':
        return wl.Wolfram.QuantumFramework.QuantumOperator('Reset', order),
    elif gate.name == 'barrier':
        return 'Barrier'
    elif hasattr(gate, 'num_ctrl_qubits'):
        arg = ['C', gate_to_QuantumOperator(gate.base_gate, order[gate.num_ctrl_qubits:]), reverse_binary(gate.ctrl_state, gate.num_ctrl_qubits), order[:gate.num_ctrl_qubits]]
        return wl.Wolfram.QuantumFramework.QuantumOperator(arg)
    else:
        from qiskit.quantum_info import Operator
        return wl.Wolfram.QuantumFramework.QuantumOperator(Operator(gate).to_matrix(), [order, order], wl.Rule('Label', gate.name if gate.name else None))

def qc_to_QuantumCircuitOperator(qc, label=None):
    ops = []
    for gate, qubits, clbits in qc:
        order = []
        for q in qubits:
            size = 0
            for r in qc.qubits:
                if r._register._name == q._register._name:
                    order.append(size + q._index + 1)
                    break
                else:
                    size = r._index + 1
        try: 
            if isinstance(gate, qiskit.qasm2.parse._DefinedGate):
                sub_qc = QuantumCircuit(max(order), gate.num_clbits)
                sub_qc.append(gate, [o - 1 for o in order])
                ops.append(qc_to_QuantumCircuitOperator(sub_qc.decompose(), gate.name))
            else:
                ops.append(gate_to_QuantumOperator(gate, order))
        except:
            ops.append(gate_to_QuantumOperator(gate, order))
    return wl.Wolfram.QuantumFramework.QuantumCircuitOperator(ops, label if label else None)

qc_to_QuantumCircuitOperator(qc)
"]
]


qiskitMatrix[qc_QiskitCircuit] := Block[{$pythonBytes = qc["Bytes"]},
    PythonEvaluate[Context[$pythonBytes], "
from qiskit.quantum_info import Operator

import pickle

qc = pickle.loads(<* $pythonBytes *>)
qc.remove_final_measurements()
Operator(qc).data
"]
]

Options[qiskitInitBackend] = {"Provider" -> None, "Backend" -> Automatic, "FireOpal" -> False}

qiskitInitBackend[qc_QiskitCircuit, OptionsPattern[]] := Enclose @ Block[{
    $pythonBytes = qc["Bytes"],
    provider, params,
    $backendName = Replace[OptionValue["Backend"], Automatic -> Null],
    $fireOpal = TrueQ[OptionValue["FireOpal"]],
    $token = Null,
    env
},
    env = If[$fireOpal, "qctrl", Automatic];
    {provider, params} = Replace[OptionValue["Provider"], {
        {name_, params : OptionsPattern[]} | name_ :> {name, Flatten[{params}]}
    }];
    If[provider === None && $fireOpal, provider = "IBMProvider"];

    If[ MatchQ[provider, "IBMQ" | "IBMProvider"],
        $token = Lookup[
            params,
            "Token",
            Enclose[Confirm @ Lookup[Last[ConfirmMatch[ServiceConnections`Private`serviceAuthentication[ServiceConnect["IBMQ"]["ID"]], _KeyClient`KeyToken]], "Token"], Null &]
        ];
    ];

    Confirm @ PythonEvaluate[Context[$pythonBytes], Switch[provider,
    "IBMQ",
"
from qiskit.providers.ibmq.accountprovider import AccountProvider
try:
    assert(isinstance(provider, AccountProvider))
except:
    from qiskit import IBMQ
    token = <* $token *>
    if token is not None:
        from qiskit_ibm_runtime import QiskitRuntimeService
        QiskitRuntimeService.save_account(channel='ibm_quantum', token=token, overwrite=True)
    provider = IBMQ.load_account()
",
    "IBMProvider",
"
from qiskit_ibm_provider import IBMProvider
try:
    assert(isinstance(provider, IBMProvider))
except:
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
    provider = AWSBraketProvider()
",
    _,
"
provider = None
"], env];
    PythonEvaluate[Context[$pythonBytes], "
import pickle
from qiskit.providers.basic_provider import BasicProvider
from qiskit_aer import AerSimulator
from qiskit_braket_provider import AWSBraketProvider
from qiskit_ibm_provider import IBMProvider

qc = pickle.loads(<* $pythonBytes *>)
backend_name = <* $backendName *>
if provider is None:
    provider = BasicProvider()
    backend = AerSimulator()
else:
    if backend_name is None:
        if isinstance(provider, AWSBraketProvider):
            backend = provider.get_backend('SV1')
        else:
            backend = provider.get_backend('ibmq_qasm_simulator')
    else:
        backend = provider.get_backend(backend_name)
if <* $fireOpal *>:
    import fireopal
    from fireopal.credentials import make_credentials_for_ibmq, make_credentials_for_braket
    if isinstance(provider, IBMProvider):
        fireopal_credentials = make_credentials_for_ibmq(provider._account.token, 'open', 'ibm-q', 'main')
    else:
        raise ValueError(f'Unsupported FireOpal provider: {provider}')
", env]
]

Options[qiskitApply] = Join[{"Shots" -> 1024, "Validate" -> False}, Options[qiskitInitBackend]]

qiskitApply[qc_QiskitCircuit, qs_QuantumState, opts : OptionsPattern[]] := Enclose @ Block[{
    $state = If[qs["Dimension"] == 1, Null, NumericArray @ N @ qs["Reverse"]["StateVector"]],
    $shots = OptionValue["Shots"],
    $fireOpal = TrueQ[OptionValue["FireOpal"]],
    $validate = TrueQ[OptionValue["Validate"]],
    env, result
},
    env = If[$fireOpal, "qctrl", Automatic];
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

state = <* $state *>
if state is not None and not isinstance(provider, AWSBraketProvider):
    circuit.initialize(state)
circuit = circuit.compose(qc)

try:
    circuit = transpile(circuit, backend)
except:
    pass

if <* $fireOpal *>:
    from qiskit import qasm2
    import fireopal
    qasm = qasm2.dumps(circuit)
    if <* $validate *>:
        validate_results = fireopal.validate(
            circuits=[qasm], credentials=fireopal_credentials, backend_name=backend.name
        )
        assert validate_results['results'] == [], validate_results['results'][0]['error_message']
    result = fireopal.execute(
        circuits=[qasm],
        shot_count=<* $shots *>,
        credentials=fireopal_credentials,
        backend_name=backend.name,
    )['results'][0]
    result = {k: v for k, v in result.items()}
else:

    if isinstance(provider, AWSBraketProvider):
        result = backend.run(circuit, shots = <* $shots *>).result()
        result = result.get_counts()
    else:
        if qc.num_clbits > 0:
            result = backend.run(circuit, shots = <* $shots *>).result()
            result = result.get_counts(circuit)
        else:
            from qiskit.quantum_info import Statevector
            result = Statevector(qc).data
result
", env];
    Which[
        AssociationQ[result],
        Block[{counts = KeyMap[StringDelete[Whitespace]] @ result, size},
            Enclose[
                size = StringLength @ First @ Keys[counts];
                ConfirmAssert[size <= 8];
                ConfirmBy[
                    QuantumMeasurement[
                        Join[
                            Association[# -> 0 & /@ IntegerDigits[Range[2 ^ size] - 1, 2, size]],
                            KeyMap[Reverse[Characters[#]] /. {"0" -> 0, "1" -> 1} &] @ counts
                        ]
                    ],
                    QuantumMeasurementQ
                ],
                counts &
            ]
        ],
        NumericArrayQ[result],
        QuantumState[Chop @ Normal[result]]["Reverse"],
        True,
        result
    ]
]

qc_QiskitCircuit["Decompose", n : _Integer ? Positive : 1] := QiskitCircuit[qc["EvalBytes", "decompose", "reps" -> n]]

qc_QiskitCircuit["QuantumOperator"] := Enclose @ Module[{mat = Chop @ Normal[ConfirmBy[qiskitMatrix[qc], NumericArrayQ]]},
    ConfirmAssert[SquareMatrixQ[mat]];
    QuantumOperator[mat]["Reverse"]["Sort"]
]

qc_QiskitCircuit["Matrix"] := qc["QuantumOperator"]["Matrix"]

qc_QiskitCircuit[opts : OptionsPattern[qiskitApply]] := qiskitApply[qc, QuantumState[{"Register", qc["Qubits"]}], opts]

qc_QiskitCircuit[qs_QuantumState, opts : OptionsPattern[qiskitApply]] := qiskitApply[qc, qs, opts]


qiskitQASM[qc_, opts : OptionsPattern[Join[{"Version" -> 2}, Options[qiskitInitBackend]]]] := Enclose @ Block[{$version = OptionValue["Version"]},
    Confirm @ qiskitInitBackend[qc, FilterRules[{opts}, Options[qiskitInitBackend]]];
    PythonEvaluate[Context[$version], "
from qiskit import transpile
from qiskit_braket_provider import AWSBraketProvider

circuit = transpile(qc, backend)
if isinstance(provider, AWSBraketProvider):
    from qiskit_braket_provider.providers.adapter import convert_qiskit_to_braket_circuit
    from braket.circuits.serialization import IRType
    result = convert_qiskit_to_braket_circuit(circuit).to_ir(IRType.OPENQASM).source
else:
    if <* $version *> == 3:
        from qiskit import qasm3
        result = qasm3.dumps(circuit)
    else:
        from qiskit import qasm2
        result = qasm2.dumps(circuit)
result
"]
]

qc_QiskitCircuit["QASM" | "QASM2", opts___] := qiskitQASM[qc, "Version" -> 2, opts]

qc_QiskitCircuit["QASM3", opts___] := qiskitQASM[qc, "Version" -> 3, opts]


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

qc_QiskitCircuit["Transpile", basisGates : {_String...} | None : None, opts : OptionsPattern[qiskitInitBackend]]:= Enclose @ Block[{
    $basisGates = Replace[basisGates, None -> Null]
},
    Confirm @ qiskitInitBackend[qc, opts];
    PythonEvaluate[Context[$basisGates], "
import pickle
from qiskit import transpile
from wolframclient.language import wl

wl.Wolfram.QuantumFramework.QiskitCircuit(pickle.dumps(transpile(qc, backend=backend, basis_gates=<* $basisGates *>)))
"]
]

qc_QiskitCircuit["Validate", opts : OptionsPattern[qiskitInitBackend]]:= Enclose[
    Confirm @ qiskitInitBackend[qc, opts, "FireOpal" -> True, "Provider" -> "IBMProvider"];
    PythonEvaluate["
import fireopal
from qiskit import transpile, qasm2
circuit = transpile(qc, backend)
qc_qasm = qasm2.dumps(qc)
circuit_qasm = qasm2.dumps(circuit)
fireopal.validate(
    circuits=[qc_qasm, circuit_qasm], credentials=fireopal_credentials, backend_name=backend.name
)
", "qctrl"]
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

