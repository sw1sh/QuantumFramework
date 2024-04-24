Package["Wolfram`QuantumFramework`"]

PackageScope["ClassiqQuantumState"]



Options[ClassiqQuantumState] = {"Bound" -> 0.01}

ClassiqQuantumState[qs_QuantumState, OptionsPattern[]] := Enclose @ Block[{
    probabilities = NumericArray[qs["ProbabilitiesList"]],
    bound = ConfirmBy[OptionValue["Bound"], RealValuedNumberQ],
    qasm
},

    qasm = Confirm @ PythonEvaluate[Context[probabilities], "
from classiq import (
    authenticate,
    Output,
    QArray,
    QBit,
    qfunc,
    create_model,
    prepare_state,
    synthesize
)
import json

authenticate()

probabilities = <* probabilities *>

probabilities = probabilities / sum(probabilities)

@qfunc
def main(io: Output[QArray[QBit]]):
    prepare_state(probabilities=list(probabilities), bound=<* bound *>, out=io)

model = create_model(main)

qprog = synthesize(model)

json.loads(qprog)['outputs']['qasm']
"];
    ImportQASMCircuit[qasm]["QuantumCircuit"]
]