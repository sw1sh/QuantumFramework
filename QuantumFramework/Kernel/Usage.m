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
\"[\", RowBox[{\"{\", RowBox[{\"\\\"\\!\\(\\*StyleBox[\\\"name\\\", \
\\\"TI\\\"]\\)\\\"\", \",\", StyleBox[\"d\", \"TI\"]}], \"}\"}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a \", Cell[BoxData[StyleBox[\"d\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \"-dimensional \
version of a named quantum basis \", \
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
\"-dimensional).\"}]]}}]], \"Usage\", Rule[CellID, 721227442]]\)"



QuantumChannel::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumChannel\", \"[\", \
RowBox[{StyleBox[\"ko\", \"TI\"], \",\", StyleBox[\"order\", \"TI\"], \",\", \
StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]represents a quantum channel with \
Kraus operators \", Cell[BoxData[StyleBox[\"ko\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \", in basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", to be applied onto the qubits indexed in \", \
Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumChannel\", \"[\", \
RowBox[{StyleBox[\"name\", \"TI\"], \",\", StyleBox[\"par\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a the named quantum channel with name \", \
Cell[BoxData[StyleBox[\"name\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", and potential parameters specified by \", \
Cell[BoxData[StyleBox[\"par\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, 14939724]]\)"



QuantumCircuitOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{\"QuantumCircuitOperator\", \"[\", \
StyleBox[\"qgates\", \"TI\"], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" \\[LineSeparator]represents a quantum circuit with \
list of quantum discrete operators or quantum measurement operators \", \
Cell[BoxData[StyleBox[\"qgates\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, \
982511436]]\)"



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
\\[LineSeparator]computes the entanglement monotone (measure) with metric \", \
Cell[BoxData[StyleBox[\"t\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" on the quantum state \", \
Cell[BoxData[StyleBox[\"qs\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" between subsystems on bipartition list \", \
Cell[BoxData[StyleBox[\"bipart\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, \
10060029]]\)"



QuantumMeasurement::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurement\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurement\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"dist\", \"TI\"], \",\", \
StyleBox[\"s\", \"TI\"]}], \"]\"}]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \"\\[LineSeparator]represents the result of a quantum \
measurement described by an association of outcomes with their corresponding \
probabilities \", Cell[BoxData[StyleBox[\"dist\", \"TI\"]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" and the list \
\", Cell[BoxData[StyleBox[\"s\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" of possible quantum states after \
the measurement.\"}]]}}]], \"Usage\", Rule[CellID, 27411816]]\)"



QuantumMeasurementOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"op\", \"TI\"], \",\", StyleBox[\"order\", \"TI\"], \
\",\", StyleBox[\"t\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a measurement operator given by op that acts on \
a state at the qubits indexed in \", Cell[BoxData[StyleBox[\"order\", \
\"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" of \
type t in the discrete quantum basis \", Cell[BoxData[StyleBox[\"qb\", \
\"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"matrix\", \"TI\"], \",\", StyleBox[\"order\", \
\"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents a \
measurement operator with matrix representation \", \
Cell[BoxData[StyleBox[\"matrix\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", in the discrete quantum basis \", \
Cell[BoxData[StyleBox[\"qb\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", that acts on a state at the qubits indexed in \", \
Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{RowBox[{StyleBox[\"basis\", \"TI\"], \"\[Rule]\", \
StyleBox[\"eig\", \"TI\"]}], \",\", StyleBox[\"order\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents a measurement with respect to the \", \
Cell[BoxData[TemplateBox[List[Cell[TextData[\"QuantumBasis\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumBasis\", \"Wolfram Package \
Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \", \
Cell[BoxData[StyleBox[\"basis\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \", with results eigenvalues \", \
Cell[BoxData[StyleBox[\"eig\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", that acts on a state at the qubits indexed in \", \
Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumM\
easurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", RowBox[{StyleBox[\"qm\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \
\"]\"}]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]changes the basis of the \", \
Cell[BoxData[TemplateBox[List[Cell[TextData[\"QuantumMeasurementOperator\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumMeasurementOperator\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \" \", \
Cell[BoxData[StyleBox[\"qm\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \", to the basis \", Cell[BoxData[StyleBox[\"qb\", \
\"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\".\"}]]}}]], \"Usage\", Rule[CellID, 186550161]]\)"



QuantumOperator::usage = "\!\(\*Cell[BoxData[GridBox[{{\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumO\
perator\"]], \"paclet:Wolfram/QuantumFramework/ref/QuantumOperator\", \
\"Wolfram Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \
\"InlineFormula\"]], \"[\", RowBox[{StyleBox[\"rep\", \"TI\"], \",\", \
StyleBox[\"order\", \"TI\"], \",\", StyleBox[\"qb\", \"TI\"]}], \"]\"}]], \
\"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\"\\[LineSeparator]represents an operator with matrix representation \", \
Cell[BoxData[StyleBox[\"rep\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \" that acts on a state at the qubits indexed in \", \
Cell[BoxData[StyleBox[\"order\", \"TI\"]], \"InlineFormula\", \
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
\"Source Sans Pro\"]], \" to the discrete quantum basis \", \
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
\"[\", StyleBox[\"name\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]represents the \
named quantum state identified by \", Cell[BoxData[StyleBox[\"name\", \
\"TI\"]], \"InlineFormula\", Rule[FontFamily, \"Source Sans Pro\"]], \
\".\"}]]}, {\"\", \
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
\"[\", StyleBox[\"qds\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \" \\[LineSeparator]gives the tensor \
product of the quantum states in the list or sequence \", \
Cell[BoxData[StyleBox[\"qss\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumT\
ensorProduct\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumTensorProduct\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"qbs\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]gives the tensor \
product of quantum bases in the list or sequence \", \
Cell[BoxData[StyleBox[\"qbs\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumT\
ensorProduct\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumTensorProduct\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"qdo\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]gives the tensor \
product of the quantum discrete operators in the list or sequence \", \
Cell[BoxData[StyleBox[\"qdo\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}, {\"\", \
Cell[TextData[{Cell[BoxData[RowBox[{TemplateBox[List[Cell[TextData[\"QuantumT\
ensorProduct\"]], \
\"paclet:Wolfram/QuantumFramework/ref/QuantumTensorProduct\", \"Wolfram \
Package Symbol\"], \"PackageLink\", Rule[BaseStyle, \"InlineFormula\"]], \
\"[\", StyleBox[\"qmo\", \"TI\"], \"]\"}]], \"InlineFormula\", \
Rule[FontFamily, \"Source Sans Pro\"]], \"\\[LineSeparator]gives the tensor \
product of the quantum measurement operators in the list or sequence \", \
Cell[BoxData[StyleBox[\"qmo\", \"TI\"]], \"InlineFormula\", Rule[FontFamily, \
\"Source Sans Pro\"]], \".\"}]]}}]], \"Usage\", Rule[CellID, 244759856]]\)"



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
