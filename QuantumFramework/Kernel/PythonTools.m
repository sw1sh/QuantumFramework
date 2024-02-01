Package["Wolfram`QuantumFramework`"]

PackageScope["PythonEvalAttr"]
PackageScope["$PythonSession"]
PackageScope["PythonEvaluate"]



$PythonPackages = {"wolframclient", "qiskit", "matplotlib", "pylatexenc", "qiskit-ibm-provider", "qiskit-braket-provider", "fire-opal", "pyzx"}

$PythonSession := SelectFirst[
    ExternalSessions["Python"],
    #["ID"] == "QuantumFramework" &,
    With[{versions = Through[PacletFind["ExternalEvaluate"]["Version"]], required = "32.2"},
        If[ AllTrue[versions, ResourceFunction["VersionOrder"][#, required] < 0 &], 
            Failure["DependencyFailure", <|
                "MessageTemplate" ->  "ExternalEvaluate paclet should be at least version `` (availabe in Wolfram Language 14), but only versions {``} are found", 
                "MessageParameters" -> {required, StringRiffle[versions, ", "]}
            |>],
            StartExternalSession[{{"Python", "StandardErrorFunction" -> Null},
                "ID" -> "QuantumFramework",
                "Evaluator" -> <|"Dependencies" -> $PythonPackages, "PythonRuntime" -> "3.11"|>,
                "SessionProlog" -> "import qiskit\nfrom qiskit import QuantumCircuit"
            }]
        ]
    ]
]


PythonEvaluate[code_] := Enclose @ With[{session = Confirm @ $PythonSession},
    Progress`EvaluateWithProgress[
        ExternalEvaluate[session, code],
        <|"Text" -> "Running Python code", "ElapsedTime" -> Automatic|>
    ]
]

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

