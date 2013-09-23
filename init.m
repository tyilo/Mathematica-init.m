(* ::Package:: *)

(** User Mathematica initialization file **)

Begin["System`"]

SinDeg[d_] := Sin[d * Degree]
CosDeg[d_] := Cos[d * Degree]
TanDeg[d_] := Tan[d * Degree]

ArcSinDeg[d_] := ArcSin[d] / Degree
ArcCosDeg[d_] := ArcCos[d] / Degree
ArcTanDeg[d_] := ArcTan[d] / Degree

CenterDot = Times

PlusMinus[{a1_, a2_}, {b1_, b2_}] := {a1 + b1, a2 - b2}
PlusMinus[{a1_, a2_}, b_] := {a1 + b, a2 - b}
PlusMinus[a_, {b1_, b2_}] := {a + b1, a - b2}
PlusMinus[a_, b_] := If[a + b == a - b, a + b, {a + b, a - b}, {a + b, a - b}]

MinusPlus[{a1_, a2_}, {b1_, b2_}] := {a1 - b1, a2 + b2}
MinusPlus[{a1_, a2_}, b_] := {a1 - b, a2 + b}
MinusPlus[a_, {b1_, b2_}] := {a - b1, a + b2}
MinusPlus[a_, b_] := If[a + b == a - b, a + b, {a - b, a + b}, {a - b, a + b}]

InfixNotation[ParsedBoxWrapper["\[CirclePlus]"], BitXor]

PlotIntercept[f1_, f2_, o_, options_] := (solution = Solve[y == f1 && y == f2, {x, y}];
	Show[ListPlot[Transpose[{x /. solution, y /. solution}], PlotStyle -> {Red, PointSize[0.0125]}], Plot[{f1, f2}, o], options])
	
PlotIntercept[f1_, f2_, o_] := PlotIntercept[f1, f2, o, {}]

PlotDefiniteIntegral[f_, from_, to_, margin_] := Show@{
	Plot[f[x], {x, from - margin, to + margin}, AxesOrigin -> {0, 0}, Epilog -> {
		{Black, Line[{{from, 0}, {from, f[from]}}]},
		{Black, Line[{{to, 0}, {to, f[to]}}]}
	}],
	Plot[f[x], {x, from, to}, Filling -> 0]
}

ChemicalTable[formula_] := Module[{chemicals, properties},
	chemicals = Check[ChemicalData[formula, "StandardName"], Break[]];
	chemicals = If[Length[chemicals] == 0, {chemicals}, chemicals];
	properties = {"StandardName", "SMILES", "StructureDiagram"};
	OpenerView[{formula,
		Grid[
			Table[
				(Table[
					ChemicalData[#, property],
					{property, properties}
				] &)[chemical],
				{chemical, chemicals}
			],
		Frame -> All]
	}, True]
]

ManToGif[man_, name_String, step_Integer] :=
 Export[name <> ".gif",
  Import[
	Export[name <> Which[$OperatingSystem == "MacOSX", ".mov", $OperatingSystem == "Windows", ".avi"],
	 man],
	"ImageList"][[1 ;; -1 ;; step]]
  ]

numPlot[ss_,{s_,e_},ee_]:=intPlot[{{ss,{s,e},ee}}]
numPlot[ints:{{_String,{_?NumericQ,_?NumericQ},_String}..}]:=Module[{i=0,c=ColorData[1,"ColorList"]},With[{min=Min[ints[[All,2,1]]],max=Max[ints[[All,2,2]]]},Graphics[Table[With[{ss=int[[1]],s=int[[2,1]],e=int[[2,2]],ee=int[[3]]},{c[[++i+1]],Thickness[.01],Text[Style[ss,Large,c[[i+1]],Bold],{s,i}],Text[Style[ee,Large,c[[i+1]],Bold],{e,i}],Line[{{s,i},{e,i}}]}],{int,ints}],Axes->{True,False},AxesStyle->Directive[Thin,Blue,12],PlotRange->{{0,max+.2 Abs@(min-max)},{0,++i}},AspectRatio->.2]]]
(* Deca is intentionally left out as only one character prefixes are supported *)
$SIPrefixes={"Y"->"Yotta","Z"->"Zetta","E"->"Exa","P"->"Peta","T"->"Tera","G"->"Giga","M"->"Mega","k"->"Kilo","h"->"Hecto","d"->"Deci","c"->"Centi","m"->"Milli","\[Mu]"|"\[Micro]"->"Micro","n"->"Nano","p"->"Pico","f"->"Femto","a"->"Atto","z"->"Zepto","y"->"Yocto"};
$UnitAbbreviations={"\[Degree]"->"angularDegrees","\[Degree]C"->"degreesCelsius","\[CapitalOmega]"->"ohms","A"->"amperes","Bq"->"becquerels","C"->"coulombs","F"->"farads","Gy"->"grays","H"->"henries","Hz"->"hertz","J"->"joules","K"->"kelvins","L"->"liters","M"->"molar","N"->"newtons","Pa"->"pascals","S"->"siemens","Sv"->"sieverts","T"->"teslas","V"->"volts","W"->"watts","Wb"->"webers","a"->"julianYears","atm"->"atmospheres","au"->"astronomicalUnit","bar"->"bars","cd"->"candelas","d"->"days","eV"->"electronvolts","g"->"grams","h"->"hours","kat"->"katals","lm"->"lumens","lx"->"lux","m"->"meters","min"->"minutes","mol"->"moles","rad"->"radians","s"->"seconds","sr"->"steradians"};

FirstDropWhile[list_, cond_] := (
	l = LengthWhile[list,cond];
	If[l == Length[list],
		None,
		list[[l+1]]
	])
StringCapitalize[str_] := ToUpperCase @ Characters[str][[1]] <> StringDrop[str, 1]
ReplaceUnit[str_] := str /. $UnitAbbreviations
ReplaceSIPrefix[str_] := (Characters[str][[1]] /. $SIPrefixes) <> StringDrop[str, 1]

UnitFullName[str_]:=(
	transformations = {Identity, StringCapitalize,
		Composition[StringCapitalize,ReplaceUnit], ReplaceSIPrefix,
		(ReplaceSIPrefix@Characters[#][[1]]) <> ReplaceUnit[StringDrop[#,1]]&
	};
	candidates = Flatten[{#, # <> "s"}& /@ Through[transformations[str]]];
	FirstDropWhile[candidates, !KnownUnitQ@# &]
)

CurrentValue[$FrontEnd, InputAliases] = 
	Append[DeleteCases[CurrentValue[$FrontEnd, InputAliases], "qu" -> _],
	"qu" -> TemplateBox[{"\[SelectionPlaceholder]", "\[Placeholder]"}, 
	"QuantityUnit", Tooltip -> "Unit Template", 
	DisplayFunction -> (PanelBox[RowBox[{#1, StyleBox[#2, "QuantityUnitTraditionalLabel"]}], FrameMargins -> 2] &), 
	InterpretationFunction -> (With[{unit = #2 /. s_String?LetterQ :> "\""~~(UnitFullName[s])~~"\"" /. s_String :> (s /. "\[CenterDot]" -> "*")},
		If[KnownUnitQ@@MakeExpression@unit,
			RowBox[{"Quantity", "[", #1, ",", unit, "]"}],
			RowBox[{"Quantity", "[", #1, ",", "\""~~StringTake[ToString[MakeExpression@#2, InputForm], {14, -2}]~~"\"", "]"}]
		]] &)]]

SolveFunctionCoordinates[coordinates_] := (length = Length[coordinates];
	equations = {};
	variables = {};
	For[i = 1,
		i <= length,
		i++,
		variables = Append[variables, ToExpression[FromCharacterCode[96 + i]]];
		rightHand = 0;
		For[j = 1,
			j <= length,
			j++,
			rightHand += ToExpression[FromCharacterCode[96 + j]]*
			coordinates[[i]][[1]]^(length - j);
		];
		equations = Append[equations, coordinates[[i]][[2]] == rightHand]
	];
	Solve[equations, variables])
 
SetAttributes[traceViewCompact, {HoldAllComplete}];
traceViewCompact[expr_] :=
Module[{steps = {}, stack = {}, pre, post, show, default = False},
		pre[e_] := (stack = {steps, stack}; steps = {});
		post[e_,
			 r_] := (steps = First@stack~Join~{show[e, HoldForm@r, steps]};
					 stack = stack[[2]]);
		SetAttributes[post, HoldAllComplete];
		show[e_, r_, steps_] :=
		Module[{open = False},
			   Grid[steps /. {{} -> {{"Expr  ",
			Item[e, Background -> GrayLevel@.8]}}, _ -> {{"Expr  ",
				e}, {Toggler[
							 Dynamic@
							 open, {True ->
								 Button["Steps", Appearance -> {"DialogBox", "Pressed"}],
								 False -> Button@"Steps"}],
					steps /. {{} -> Style["no definitions apply", Italic], _ :>
						Dynamic@
						If[open, Column@steps,
						   Grid@{{Length@steps, "steps"}}]}}, {"Result", r}}},
					Alignment -> {Left, Center}, Frame -> All,
					Spacings -> Automatic, Background -> {{Hue[.65, .1, 1]}, None}]];
		TraceScan[pre, expr, ___, post];
		Deploy@
		Column@{Opener@Dynamic@default, 
		Dynamic@Pane[First@steps, ImageSize -> 10000]}];


Notation`AutoLoadNotationPalette = False
Needs["Notation`"]
Symbolize[ParsedBoxWrapper[SubscriptBox["_", "_"]]]

End[]
