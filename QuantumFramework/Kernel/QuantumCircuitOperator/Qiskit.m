Package["Wolfram`QuantumFramework`"]

PackageExport["QiskitCircuit"]
PackageScope["QuantumCircuitOperatorToQiskit"]



existingGateNames = {
    "barrier", "measure", "reset", "u3", "u2", "u1",
    "cx", "id", "u0", "u", "p", "x", "y", "z", "h", "s", "sdg", "t",
    "tdg", "rx", "ry", "rz", "sx", "sxdg", "cz", "cy", "swap", "ch",
    "ccx", "cswap", "crx", "cry", "crz", "cu1", "cp", "cu3", "csx",
    "cu", "rxx", "rzz", "rccx", "rc3x", "c3x", "c3sx", "c4x"
};

labelToGate = Replace[{
    "Controlled"[x_String, ___] :> "c" <> Replace[ToLowerCase[x], "not" -> "x"],
    Superscript[x_String, CircleTimes[_]] :> x,
    Superscript[x_String, "\[Dagger]"] :> ToLowerCase[x] <> "dg",
    x_String :> ToLowerCase[x]
}]

QuantumCircuitOperatorToQiskit[qco_QuantumCircuitOperator] := Enclose @ Block[{
    operators = Map[
        With[{
            label = If[QuantumMeasurementOperatorQ[#], "m", labelToGate @ #["Label"]]
        },
            Switch[
                label,
                "m",
                Splice[{label, None, {#, #}} & /@ (#["InputOrder"] - 1)],
                "cx" | "cy" | "cz",
                Module[{c1 = #["Label"][[2]], c0 = #["Label"][[3]], t = #["TargetOrder"], range},
                    range = Join[c1, c0];
                    {
                        "control",
                        {Length[c1] + Length[c0], label, StringReverse @ StringJoin @ Replace[range, Join[Thread[c1 -> "1"], Thread[c0 -> "0"]], {1}]},
                        Join[range, t] - 1
                    }
                ],
                _,
                {
                    label,
                    NumericArray @ N @ Normal @ #["MatrixRepresentation"],
                    #["InputOrder"] - 1
                }
            ]
        ] &,
        qco["Operators"]
    ],
    arity = {qco["Width"], Replace[Quiet[qco["Targets"]], Except[_Integer] -> Nothing]}
},
    ExternalEvaluate[Confirm @ $PythonSession, "
from wolframclient.language import wl

from qiskit import QuantumCircuit
from qiskit.extensions import UnitaryGate
from qiskit.circuit.gate import Gate
from qiskit.circuit.library.standard_gates import XGate, YGate, ZGate

import pickle


circuit = QuantumCircuit(
    * <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`arity *>
)

for name, data, order in <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`operators *>:
    if name.lower() in <* Wolfram`QuantumFramework`Qiskit`PackagePrivate`existingGateNames *>:
        getattr(circuit, name.lower())(*tuple(order))
    elif name == 'control':
        base_name = data[1][1:]
        if base_name == 'x':
            base_gate = XGate()
        elif base_name == 'y':
            base_gate = YGate()
        elif base_name == 'z':
            base_gate = ZGate()
        else:
            base_gate = Gate(base_name, n_qubits=data[0])
        circuit.append(base_gate.control(len(data[2]), ctrl_state=data[2]), tuple(order))
    else:
        circuit.append(UnitaryGate(data, name), tuple(order))

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
        ExternalEvaluate[Confirm @ $PythonSession, "
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


qiskitDiagram[qc_QiskitCircuit, OptionsPattern[{"Scale" -> 5}]] := Enclose @ Block[{
    Wolfram`QuantumFramework`$pythonBytes = qc["Bytes"],
    Wolfram`QuantumFramework`scale = OptionValue["Scale"]
},
    ExternalEvaluate[Confirm @ $PythonSession, "
import pickle
import PIL
import matplotlib
matplotlib.use('Agg')
fig = pickle.loads(<* Wolfram`QuantumFramework`$pythonBytes *>).draw(output='mpl', scale=<* Wolfram`QuantumFramework`scale *>)
fig.canvas.draw()
PIL.Image.frombytes('RGB', fig.canvas.get_width_height(), fig.canvas.tostring_rgb())
"]
]


qc_QiskitCircuit["Graphics", opts : OptionsPattern[]] := qiskitGraphics[qc, opts]

qc_QiskitCircuit["Diagram", opts : OptionsPattern[]] := qiskitDiagram[qc, opts]

qc_QiskitCircuit["Qubits"] := qc["Eval", "num_qubits"]

qc_QiskitCircuit["Depth"] := qc["Eval", "depth"]

qc_QiskitCircuit["Ops"] := qc["Eval", "count_ops"]

qc_QiskitCircuit["QuantumCircuit" | "QuantumCircuitOperator"] := Enclose @ Block[{
    Wolfram`QuantumFramework`$pythonBytes = qc["Bytes"]
},
    ExternalEvaluate[Confirm @ $PythonSession, "
import pickle
from wolframclient.language import wl
from math import pi

qc = pickle.loads(<* Wolfram`QuantumFramework`$pythonBytes *>)

ops = []
for gate, qubits, clbits in qc:
    order = [q.index + 1 for q in qubits]
    if len(gate.params) > 0:
        xs = []
        for x in gate.params:
            if isinstance(x, float):
                if x == pi:
                    xs.append(wl.Pi)
                elif x == - pi:
                    xs.append(wl.Minus(wl.Pi))
            else:
                xs.append(x)
    if gate.name == 'x':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('X', order))
    elif gate.name == 'h':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('H', order))
    elif gate.name == 'p':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['Phase', gate.params[0]], order))
    elif gate.name == 'u':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(['U', *gate.params], order))
    elif gate.name == 'cx':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CNOT', order))
    elif gate.name == 'cy':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CY', order))
    elif gate.name == 'cz':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('CZ', order))
    elif gate.name == 't':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('T', order))
    elif gate.name == 'tdg':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator('T', order)('Dagger'))
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
    elif gate.name == 'unitary':
        ops.append(wl.Wolfram.QuantumFramework.QuantumOperator(*xs, wl.Rule('Label', gate.label)))
    else:
        print('Uknonwn gate: ', gate.name, gate.params, [q.index for q in qubits])
wl.Wolfram.QuantumFramework.QuantumCircuitOperator(ops)
"]
]


qiskitMatrix[qc_QiskitCircuit] := Block[{Wolfram`QuantumFramework`$pythonBytes = qc["Bytes"]},
    ExternalEvaluate[$PythonSession, "
from qiskit import BasicAer, transpile

import pickle

qc = pickle.loads(<* Wolfram`QuantumFramework`$pythonBytes *>)

backend = BasicAer.get_backend('unitary_simulator')

job = backend.run(transpile(qc, backend))
job.result().get_unitary(qc, decimals=3)
"]
]

Options[qiskitApply] = {"Backend" -> "ibmq_qasm_simulator", "OptimizationLevel" -> 3}

qiskitApply[qc_QiskitCircuit, qs_QuantumState, OptionsPattern[]] := Enclose @ Block[{
    Wolfram`QuantumFramework`$pythonBytes = qc["Bytes"],
    Wolfram`QuantumFramework`$qubits = qs["OutputQudits"],
    Wolfram`QuantumFramework`$state = NumericArray @ Normal @ N @ qs["StateVector"],
    Wolfram`QuantumFramework`$opts = <|"backend_name" -> OptionValue["Backend"], "optimization_level" -> OptionValue["OptimizationLevel"]|>
},
    ConfirmAssert[qs["InputDimensions"] == {1}];
    ConfirmAssert[AllTrue[qs["OutputDimensions"], EqualTo[2]]];
    ExternalEvaluate[$PythonSession, "
import pickle
from qiskit import IBMQ, QuantumCircuit

qc = pickle.loads(<* Wolfram`QuantumFramework`$pythonBytes *>)

qubits = <* Wolfram`QuantumFramework`$qubits *>
circuit = QuantumCircuit(qubits, qubits)
circuit.initialize(<* Wolfram`QuantumFramework`$state *>)
circuit.extend(qc)

if not IBMQ._credentials:
    provider = IBMQ.load_account()
else:
    provider = IBMQ.get_provider()
provider.run_circuits(circuit, ** <* Wolfram`QuantumFramework`$opts *>).result().get_counts()
"]
]


qc_QiskitCircuit["Matrix"] := qiskitMatrix[qc]

qc_QiskitCircuit[qs_QuantumState, opts : OptionsPattern[qiskitApply]] := qiskitApply[qc, qs, opts]


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

