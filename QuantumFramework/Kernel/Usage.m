Package["Wolfram`QuantumFramework`"]


QuantumBasis::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumB\
asis\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", \"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \\\"TI\\\"]\\)\\\"\", \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]represents a named quantum basis \", \
Cell[BoxData[\"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \\\"TI\\\"]\\)\\\"\"], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumB\
asis\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"\[LeftAssociation]\", \
RowBox[{RowBox[{SubscriptBox[StyleBox[\"name\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \"\[Rule]\", SubscriptBox[StyleBox[\"b\", \"TI\"], StyleBox[\"1\", \
\"TR\"]]}], \",\", RowBox[{SubscriptBox[StyleBox[\"name\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \"\[Rule]\", SubscriptBox[StyleBox[\"b\", \"TI\"], \
StyleBox[\"2\", \"TR\"]]}], \",\", StyleBox[\"\[Ellipsis]\", \"TR\"]}], \
\"\[RightAssociation]\"}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]represents a quantum basis with \
basis elements \", Cell[BoxData[SubscriptBox[StyleBox[\"b\", \"TI\"], \
StyleBox[\"i\", \"TI\"]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans \
Pro\"]], \", having names \", Cell[BoxData[SubscriptBox[StyleBox[\"name\", \
\"TI\"], StyleBox[\"i\", \"TI\"]]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumB\
asis\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"{\", RowBox[{SubscriptBox[StyleBox[\"n\", \"TI\"], \
StyleBox[\"1\", \"TR\"]], \",\", SubscriptBox[StyleBox[\"n\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", StyleBox[\"\[Ellipsis]\", \"TR\"]}], \
\"}\"}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a \", \
Cell[BoxData[RowBox[List[SubscriptBox[StyleBox[\"n\", \"TI\"], \
StyleBox[\"1\", \"TR\"]], \"\\[Times]\", SubscriptBox[StyleBox[\"n\", \
\"TI\"], StyleBox[\"2\", \"TR\"]], \"\\[Times]\", StyleBox[\"\\[Ellipsis]\", \
\"TR\"]]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
dimensional computational basis of a composite system (many qudits).\"}]]}, \
{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumB\
asis\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"n\", \"TI\"], \",\", StyleBox[\"m\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a \", \
Cell[BoxData[SuperscriptBox[StyleBox[\"n\", \"TI\"], StyleBox[\"m\", \
\"TI\"]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
dimensional computational basis of a composite system (\", \
Cell[BoxData[StyleBox[\"m\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" qudits, each one, \", Cell[BoxData[StyleBox[\"n\", \
\"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"-dimensional).\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumB\
asis\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"{\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"n\", \"TI\"], StyleBox[\"1\", \"TR\"]], \
\",\", SubscriptBox[StyleBox[\"n\", \"TI\"], StyleBox[\"2\", \"TR\"]], \",\", \
StyleBox[\"\[Ellipsis]\", \"TR\"]}], \"}\"}], \",\", RowBox[{StyleBox[\"{\", \
\"TI\"], RowBox[{StyleBox[SubscriptBox[\"m\", StyleBox[\"1\", \"TR\"]], \
\"TI\"], StyleBox[\",\", \"TI\"], StyleBox[SubscriptBox[\"m\", \
StyleBox[\"2\", \"TR\"]], \"TI\"], StyleBox[\",\", \"TI\"], \
StyleBox[\"\[Ellipsis]\", \"TR\"]}], StyleBox[\"}\", \"TI\"]}]}], \
StyleBox[\"}\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]represents a \", \
Cell[BoxData[RowBox[List[SubscriptBox[StyleBox[\"n\", \"TI\"], \
StyleBox[\"1\", \"TR\"]], \"\\[Times]\", SubscriptBox[StyleBox[\"n\", \
\"TI\"], StyleBox[\"2\", \"TR\"]], \"\\[Times]\", StyleBox[\"\\[Ellipsis]\", \
\"TR\"]]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
dimensional computational basis output qudits, and  \", \
Cell[BoxData[RowBox[List[SubscriptBox[StyleBox[\"m\", \"TI\"], \
StyleBox[\"1\", \"TR\"]], \"\\[Times]\", SubscriptBox[StyleBox[\"m\", \
\"TI\"], StyleBox[\"2\", \"TR\"]], \"\\[Times]\", StyleBox[\"\\[Ellipsis]\", \
\"TR\"]]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
dimensional of the input qudits. Instead of dimension, one can add named \
basis, too.\"}]]}}]], \"Usage\", Rule[CellID, 721227442]]\)"



QuantumChannel::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumChannel\", \"[\", \
RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[TemplateBox[List[Cell[TextData[\"B\"]], \
\"paclet:Wolfram/QuantumFramework/ref/B\"], \"RefLink\", Rule[BaseStyle, \
List[\"InlineFormula\", \"TI\"]]], StyleBox[\"1\", \"TR\"]], \",\", \
SubscriptBox[TemplateBox[List[Cell[TextData[\"B\"]], \
\"paclet:Wolfram/QuantumFramework/ref/B\"], \"RefLink\", Rule[BaseStyle, \
List[\"InlineFormula\", \"TI\"]]], StyleBox[\"2\", \"TR\"]], \",\", \
\"\[Ellipsis]\"}], \"}\"}], \",\", StyleBox[\"order\", \"TI\"], \",\", \
StyleBox[\"basis\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]represents a quantum channel with \
Kraus operators \", Cell[BoxData[SubscriptBox[StyleBox[\"B\", \"TI\"], \
\"i\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", in  \
\", Cell[BoxData[StyleBox[\"basis\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", to be applied onto the qubits \
indexed in \", Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumChannel\", \"[\", \
StyleBox[RowBox[{RowBox[{\"name\", \"[\", \"params\", \"]\"}], \",\", \
\"...\"}], \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source \
Sans Pro\"]], \"\\[LineSeparator]represents a the named quantum channel with \
\", Cell[BoxData[StyleBox[\"name\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", and potential parameters \
specified by \", Cell[BoxData[StyleBox[\"params\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \
\"Usage\", Rule[CellID, 14939724]]\)"



QuantumCircuitMultiwayGraph::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumCircuitMultiwayGraph\", \"[\", \
StyleBox[\"qc\", Rule[FontSlant, \"Italic\"]], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" \\[LineSeparator]represents a \
multiway graph of the quantum circuit \", Cell[BoxData[StyleBox[\"qc\", \
Rule[FontSlant, \"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source \
Sans Pro\"]]}]]}}]], \"Usage\", Rule[CellID, 14939724]]\)"



QuantumCircuitOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumCircuitOperator\", \"[\", \
RowBox[{StyleBox[\"{\", \"TR\"], RowBox[{StyleBox[SubscriptBox[\"obj\", \
\"1\"], \"TI\"], StyleBox[\",\", \"TR\"], StyleBox[SubscriptBox[\"obj\", \
\"2\"], \"TI\"], StyleBox[\",\", \"TR\"], StyleBox[SubscriptBox[\"obj\", \
\"3\"], \"TI\"], StyleBox[\",\", \"TR\"], StyleBox[\"...\", \"TR\"]}], \
StyleBox[\"}\", \"TR\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]represents a quantum circuit with \
a list of quantum objects \", Cell[BoxData[StyleBox[SubscriptBox[\"obj\", \
\"i\"], \"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\", e.g., quantum operator, quantum channel, quantum state or quantum \
measurement operators.\"}]]}}]], \"Usage\", Rule[CellID, 982511436]]\)"



QuantumDistance::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumD\
istance\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumDistance\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{SubscriptBox[StyleBox[\"qs\", \"TI\"], \
StyleBox[\"1\", \"TR\"]], \",\", SubscriptBox[StyleBox[\"qs\", \"TI\"], \
StyleBox[\"2\", \"TR\"]], \",\", StyleBox[\"t\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]returns the distance between two quantum discrete states \
using measure \", Cell[BoxData[StyleBox[\"t\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, \
107665970]]\)"



QuantumEntangledQ::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
ntangledQ\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEntangledQ\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"qs\", \"TI\"], \",\", \
StyleBox[\"s\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]gives \", \
Cell[BoxData[TemplateBox[List[Cell[TextData[\"True\"]], \"paclet:ref/True\"], \
\"RefLink\", Rule[BaseStyle, List[\"InlineFormula\"]]]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" if the subsystems in the discrete \
quantum state \", Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" are entangled at bipartition list \
\", Cell[BoxData[StyleBox[\"s\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", and \", \
Cell[BoxData[TemplateBox[List[Cell[TextData[\"False\"]], \
\"paclet:ref/False\"], \"RefLink\", Rule[BaseStyle, \
List[\"InlineFormula\"]]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans \
Pro\"]], \" otherwise.\"}]]}}]], \"Usage\", Rule[CellID, 268996006]]\)"



QuantumEntanglementMonotone::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
ntanglementMonotone\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumEntanglementMonotone\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"qs\", \"TI\"], \",\", \
StyleBox[\"bipart\", \"TI\"], \",\", StyleBox[\"t\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]computes the entanglement monotone using the measure or \
metric \", Cell[BoxData[StyleBox[\"t\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" on the quantum state \", \
Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" between subsystems on bipartition list \", \
Cell[BoxData[StyleBox[\"bipart\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, \
10060029]]\)"



QuantumEvolve::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"op\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
symbolic quantum state evolved in time  with the symbolic Hamiltonian or \
Liouvillian \", Cell[BoxData[StyleBox[\"op\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", from the \
initial register state, by setting the variable \", \
Cell[BoxData[StyleBox[\"t\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" as the time \
parameter.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", StyleBox[\"qs\", \"TI\"], \
\",\", StyleBox[\"t\", \"TI\"]}], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
symbolic quantum state evolved in time  with the symbolic Hamiltonian or \
Liouvillian \", Cell[BoxData[StyleBox[\"op\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", from the \
initial state \", Cell[BoxData[\"qs\"], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", and time as \", Cell[BoxData[\"t\"], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", StyleBox[\"qs\", \"TI\"], \
\",\", RowBox[{\"{\", RowBox[{StyleBox[\"t\", \"TI\"], \",\", \
SubscriptBox[StyleBox[\"t\", \"TI\"], StyleBox[\"i\", \"TI\"]], \",\", \
SubscriptBox[StyleBox[\"t\", \"TI\"], StyleBox[\"f\", \"TI\"]]}], \"}\"}]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a numeric quantum state evolved in time  with \
the numeric Hamiltonian or Liouvillian \", Cell[BoxData[StyleBox[\"op\", \
Rule[FontSlant, \"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source \
Sans Pro\"]], \", from the initial numeric state \", \
Cell[BoxData[StyleBox[\"qs\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \",  with variable \
\", Cell[BoxData[StyleBox[\"t\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" as time in the \
range \", Cell[BoxData[StyleBox[SubscriptBox[\"t\", \"i\"], Rule[FontSlant, \
\"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
to \", Cell[BoxData[StyleBox[SubscriptBox[\"t\", \"f\"], Rule[FontSlant, \
\"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", \
TemplateBox[List[Cell[TextData[\"None\"]], \"paclet:ref/None\"], \"RefLink\", \
Rule[BaseStyle, List[\"InlineFormula\"]]], \",\", \"\[Ellipsis]\"}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents an evolution operator of the Hamiltonian or \
Liouvillian \", Cell[BoxData[StyleBox[\"op\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"L\", \"TI\"], StyleBox[\"1\", \"TR\"]], \
\",\", SubscriptBox[StyleBox[\"L\", \"TI\"], StyleBox[\"2\", \"TR\"]], \",\", \
\"\[Ellipsis]\"}], \"}\"}], \",\", \"\[Ellipsis]\"}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]specify Lindblad jump operator.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", RowBox[{RowBox[{\"{\", \
RowBox[{SubscriptBox[StyleBox[\"L\", \"TI\"], StyleBox[\"1\", \"TR\"]], \
\",\", SubscriptBox[StyleBox[\"L\", \"TI\"], StyleBox[\"2\", \"TR\"]], \",\", \
\"\[Ellipsis]\"}], \"}\"}], \"\[Rule]\", RowBox[{\"{\", \
RowBox[{SubscriptBox[\"\[Gamma]\", \"1\"], \",\", SubscriptBox[\"\[Gamma]\", \
\"2\"], \",\", \"\[Ellipsis]\"}], \"}\"}]}], \",\", \"\[Ellipsis]\"}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]specify gamma damping rates. \"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"\[Ellipsis]\", \",\", \
RowBox[{\"\\\"AdditionalEquations\\\"\", \"->\", \"spec\"}]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents an evolution, given some additional \
specifications especially for piecewise or hybrid systems.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"\[Ellipsis]\", \",\", StyleBox[\"opt\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents an evolution, given options specified by \", \
Cell[BoxData[StyleBox[\"opt\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", which can be any options similar to \", \
ButtonBox[\"NDSolve\", Rule[BaseStyle, \"Link\"], Rule[ButtonData, \
\"paclet:guide/NDSolve\"]], \" or \", ButtonBox[\"DSolve\", Rule[BaseStyle, \
\"Link\"], Rule[ButtonData, \"paclet:guide/DSolve\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumE\
volve\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumEvolve\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{\"\[Ellipsis]\", \",\", RowBox[{\"\\\"ReturnEquations\\\"\", \
\"->\", \"True\"}]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source \
Sans Pro\"]], \"\\[LineSeparator]returns differential equations, \
corresponding to the desired dynamics specified.\"}]]}}]], \"Usage\", \
Rule[CellID, 14939724]]\)"



QuantumMeasurement::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurement\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurement\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", \"...\", \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents the \
result of quantum measurement, in a non-demolishing way.\"}]]}}]], \"Usage\", \
Rule[CellID, 27411816]]\)"



QuantumMeasurementOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"matrix\", \"TI\"], \",\", StyleBox[\"target\", \
\"TI\"], \",\", StyleBox[\"basis\", \"TI\"]}], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
measurement operator with matrix representation \", \
Cell[BoxData[StyleBox[\"matrix\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", in the quantum basis \", \
Cell[BoxData[StyleBox[\"basis\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", that acts at the qubits indexed \
in \", Cell[BoxData[StyleBox[\"target\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"basis\", \"TI\"], \",\", StyleBox[\"target\", \
\"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans \
Pro\"]], \"\\[LineSeparator]represents a measurement operator acting at the \
qubits indexed in \", Cell[BoxData[StyleBox[\"target\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" in the quantum \
basis \", Cell[BoxData[StyleBox[\"basis\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{RowBox[{StyleBox[\"basis\", \"TI\"], \"\[Rule]\", \
StyleBox[\"eig\", \"TI\"]}], \",\", StyleBox[\"target\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a measurement with respect to the \", \
Cell[BoxData[StyleBox[\"basis\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", with results eigenvalues \", \
Cell[BoxData[StyleBox[\"eig\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", that acts at the qubits indexed in \", \
Cell[BoxData[StyleBox[\"target\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", \
RowBox[{RowBox[{RowBox[{TemplateBox[List[Cell[TextData[\"QuantumOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \"[\", \
\"...\", \"]\"}], \"[\", \"\\\"Diagonalize\\\"\", \"]\"}], StyleBox[\",\", \
\"TI\"], StyleBox[\"...\", \"TI\"]}], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
measurement operator in the basis of quantum operator's eigenstates, with its \
corresponding eigenvalues.\"}]]}}]], \"Usage\", Rule[CellID, 186550161]]\)"



QuantumMeasurementSimulation::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumMeasurementSimulation\", \"[\", \
RowBox[{StyleBox[\"state\", \"TI\", Rule[FontSlant, \"Italic\"]], \",\", \
StyleBox[\"qmo\", \"TI\", Rule[FontSlant, \"Italic\"]], \",\", \
StyleBox[\"counts\", \"TI\", Rule[FontSlant, \"Italic\"]]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]simulates quantum measurement results for a list of quantum \
measurement operators (\", Cell[BoxData[StyleBox[\"qmo\", Rule[FontSlant, \
\"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \") \
given a quantum state \", Cell[BoxData[StyleBox[\"state\", \"TI\", \
Rule[FontSlant, \"Italic\"]]], \"InlineFormula\", Rule[FontFamily, \"Source \
Sans Pro\"]], \", for a given number of results (repetitions) \", \
Cell[BoxData[StyleBox[\"counts\", \"TI\", Rule[FontSlant, \"Italic\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \
\"Usage\", Rule[CellID, 14939724]]\)"



QuantumMPS::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumMPS\", \"[\", \
RowBox[{\"QuantumState\", \"[\", \"...\", \"]\"}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]returns the corresponding Matrix-Product-State, as a \
QuantumCircuitOperator object\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumMPS\", \"[\", \
RowBox[{\"QuantumOperator\", \"[\", \"...\", \"]\"}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]returns the corresponding Matrix-Product-Operator, as a \
QuantumCircuitOperator object\"}]]}}]], \"Usage\", Rule[CellID, \
587635621]]\)"



QuantumOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumO\
perator\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"rep\", \"TI\"], \",\", \
StyleBox[\"order\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents an operator with matrix/tensor representation \
\", Cell[BoxData[StyleBox[\"rep\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" that acts on a state at the qubits \
indexed in \", Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", in the quantum basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumO\
perator\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{\"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \
\\\"TI\\\"]\\)\\\"\", \",\", StyleBox[\"order\", \"TI\"], \",\", \
StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]represents the named operator \", \
Cell[BoxData[\"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \\\"TI\\\"]\\)\\\"\"], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" that acts on a \
state at the qubits indexed in \", Cell[BoxData[StyleBox[\"order\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", in the \
discrete quantum basis \", Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumO\
perator\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"qo\", \"TI\"], \",\", \
StyleBox[\"basis\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]changes the basis of the \", \
Cell[BoxData[TemplateBox[List[Cell[TextData[\"QuantumOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \", \
Cell[BoxData[StyleBox[\"qo\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" to the quantum basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, 37818702]]\)"



QuantumPartialTrace::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumP\
artialTrace\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumPartialTrace\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"qs\", \"TI\"], \",\", \
StyleBox[\"s\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]gives the quantum discrete state \
\", Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" with qubits at indices in list \", \
Cell[BoxData[StyleBox[\"s\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" traced out.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumP\
artialTrace\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumPartialTrace\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"qb\", \"TI\"], \",\", \
StyleBox[\"s\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]gives the quantum basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", with the bases at indices in list \", \
Cell[BoxData[StyleBox[\"s\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" traced out.\"}]]}}]], \"Usage\", Rule[CellID, \
82448430]]\)"



QuantumPhaseSpaceTransform::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumPhaseSpaceTransform\", \"[\", \
RowBox[{StyleBox[\"object\", \"TI\"], \",\", StyleBox[\"basis\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]represents the transformation of quantum object into the \
phase space basis\"}]]}}]], \"Usage\", Rule[CellID, 361239949]]\)"



QuantumShortcut::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumShortcut\", \"[\", \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \
\\[LineSeparator]represents the shorthand version of quantum \
objects\"}]]}}]], \"Usage\", Rule[CellID, 98385100]]\)"



QuantumStateEstimate::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{RowBox[{RowBox[{\"QuantumStateEstimate\", \
\"[\", RowBox[{\"\[LeftAssociation]\", \
RowBox[{RowBox[{SubscriptBox[StyleBox[\"qmo\", \"TI\"], StyleBox[\"1\", \
\"TR\"]], \"\[Rule]\", SubscriptBox[StyleBox[\"result\", \"TI\"], \
StyleBox[\"1\", \"TR\"]]}], \",\", RowBox[{SubscriptBox[StyleBox[\"qmo\", \
\"TI\"], StyleBox[\"2\", \"TR\"]], \"\[Rule]\", \
SubscriptBox[StyleBox[\"result\", \"TI\"], StyleBox[\"2\", \"TR\"]]}], \",\", \
\"...\"}]}]}], \"|>\"}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]represents a quantum state \
estimate with quantum measurement operators \", \
Cell[BoxData[SubscriptBox[StyleBox[\"qmo\", \"TI\"], \"i\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", and the \
experimental (or simulated) results \", \
Cell[BoxData[SubscriptBox[StyleBox[\"result\", \"TI\"], \"i\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \
\"Usage\", Rule[CellID, 14939724]]\)"



QuantumState::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumS\
tate\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumState\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"qs\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a quantum state specified by the state vector \
or density matrix \", Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", in the quantum \
basis \", Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumS\
tate\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumState\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"qs\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
quantum state specified by the state vector or density matrix \", \
Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", in the computational basis.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumS\
tate\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumState\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"asso\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a quantum state specified by the association \
\", Cell[BoxData[StyleBox[\"asso\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", in the quantum basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumS\
tate\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumState\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", \"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \\\"TI\\\"]\\)\\\"\", \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents the named quantum state identified by \", \
Cell[BoxData[StyleBox[\"name\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumS\
tate\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumState\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{RowBox[{\"QuantumState\", \"[\", RowBox[{\"...\", \",\", \
StyleBox[\"qb1\", \"TI\"]}], \"]\"}], \",\", StyleBox[\"qb2\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]changes the basis from the  quantum basis \", \
Cell[BoxData[StyleBox[\"qb1\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" to  \", Cell[BoxData[StyleBox[\"qb2\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" .\"}]]}}]], \
\"Usage\", Rule[CellID, 383074010]]\)"



QuantumTensorProduct::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumT\
ensorProduct\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumTensorProduct\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"objects\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" \\[LineSeparator]gives the tensor \
product of the quantum objects in the list or sequence \", \
Cell[BoxData[StyleBox[\"objects\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, \
244759856]]\)"



QuantumWignerMICTransform::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumWignerMICTransform\", \"[\", \
StyleBox[\"obj\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]transforms \", StyleBox[\"obj\", \
\"TI\"], \" into a minimal informationally complete basis\"}]]}}]], \
\"Usage\", Rule[CellID, 14939724]]\)"



QuantumWignerTransform::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumWignerTransform\", \"[\", \
StyleBox[\"obj\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]transforms a quantum object into \
its phase-space representation in the Wigner basis.\"}]]}}]], \"Usage\", \
Rule[CellID, 37818702]]\)"



QuditBasis::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuditBas\
is\"]], \"paclet:Wolfram/QuantumFramework/ref/QuditBasis\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \"[\", \
StyleBox[\"names\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]an association with keys as \", \
Cell[BoxData[StyleBox[\"names\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" and values as the corresponding \
tensor representation.\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuditBas\
is\"]], \"paclet:Wolfram/QuantumFramework/ref/QuditBasis\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \"[\", \
StyleBox[\"dim\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]basis of one or many qudits given \
the info about \", Cell[BoxData[StyleBox[\"dim\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \
\"Usage\", Rule[CellID, 14939724]]\)"



QuditName::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuditNam\
e\"]], \"paclet:Wolfram/QuantumFramework/ref/QuditBasis\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \"[\", \
StyleBox[\"names\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]a convenient wrapper around qudit \
names with special formatting\"}]]}}]], \"Usage\", Rule[CellID, 14939724]]\)"
