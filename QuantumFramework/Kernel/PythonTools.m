Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]
PackageScope["PythonEvaluate"]



$PythonPackages = {"qiskit", "matplotlib", "pylatexenc", "qiskit-ibm-provider", "qiskit-braket-provider"}

confirmPython[session_] := confirmPython[session] = AllTrue[$PythonPackages, ResourceFunction["PythonPackageInstalledQ"][session, #] &] &&
    ExternalEvaluate[session, "import qiskit\nfrom qiskit import QuantumCircuit"] === Null

$PythonSession := Block[{session},
Enclose[
    SelectFirst[ExternalSessions[], confirmPython, ConfirmBy[
        session = StartExternalSession["Python"],
        confirmPython,
        "No qiskit package found. Installing it first instead."
    ]],
    Enclose[
        PrintTemporary["Installing Python Packages ..."];
        Confirm[ResourceFunction["PythonPackageInstall"][session, $PythonPackages]];
        ConfirmBy[session, confirmPython, "Failed to install qiskit."]
    ] &
]
]


PythonEvaluate[code_] := Enclose @ ExternalEvaluate[Confirm @ $PythonSession, code]

PythonEvaluate[ctx_, code_] := WithCleanup[
    PrependTo[$ContextPath, ctx],
    PythonEvaluate[code],
    $ContextPath = DeleteCases[$ContextPath, ctx]
]


PythonEvalAttr[{bytes_ByteArray, attr_String, args___, kwargs : OptionsPattern[]}, OptionsPattern[{"ReturnBytes" -> False}]] := Block[{
    pythonBytes = bytes,
    pythonAttr = attr,
    pythonArgs = {args},
    pythonKWargs = Association @ {kwargs},
    returnBytes = OptionValue["ReturnBytes"]
},
    PythonEvaluate[Context[pythonBytes], "
import pickle

attr = getattr(pickle.loads(<* pythonBytes *>), <* pythonAttr *>)
if hasattr(attr, '__call__'):
    result = attr(* <* pythonArgs *>, ** <* pythonKWargs *>)
else:
    result = attr

if <* TrueQ @ returnBytes  *>:
    result = pickle.dumps(result)

result
    "]
]

