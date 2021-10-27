Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]



confirmPython[session_] := ResourceFunction["PythonPackageInstalledQ"][session, "qiskit"] || ExternalEvaluate[session, "import qiskit"] === Null

$PythonSession := With[{session = SelectFirst[ExternalSessions[], confirmPython, StartExternalSession["Python"]]},
Enclose[
    ConfirmBy[
        session,
        confirmPython,
        "No qiskit package found. Installing it first instead."
    ],
    (PrintTemporary["Installing Qiskit ..."]; ResourceFunction["PythonPackageInstall"][session, "qiskit"]; session) &
]
]


PythonEvalAttr[{bytes_ByteArray, attr_String, args___, kwargs : OptionsPattern[]}, OptionsPattern[{"ReturnBytes" -> False}]] := Block[{
    Wolfram`QuantumFramework`pythonBytes = bytes,
    Wolfram`QuantumFramework`pythonAttr = attr,
    Wolfram`QuantumFramework`pythonArgs = {args},
    Wolfram`QuantumFramework`pythonKWargs = Association @ {kwargs},
    Wolfram`QuantumFramework`returnBytes = OptionValue["ReturnBytes"]
},
    ExternalEvaluate[$PythonSession, "
import pickle

attr = getattr(pickle.loads(<* Wolfram`QuantumFramework`pythonBytes *>), <* Wolfram`QuantumFramework`pythonAttr *>)
if hasattr(attr, '__call__'):
    result = attr(* <* Wolfram`QuantumFramework`pythonArgs *>, ** <* Wolfram`QuantumFramework`pythonKWargs *>)
else:
    result = attr

if <* TrueQ @ Wolfram`QuantumFramework`returnBytes  *>:
    result = pickle.dumps(result)

result
    "]
]

