(* ::Package:: *)
(* Timestamp: 2013-09-23 23:51 *)

(** User Mathematica initialization file **)

autoUpdate = False;

If[autoUpdate,
	initPath = ToFileName[{$UserBaseDirectory, "Kernel"}, "init.m"];
	current = StringJoin@ReadList[f = OpenRead@initPath, Character];
	Close@f;
	newest = Quiet@URLFetch@"https://raw.github.com/Tyilo/Mathematica-init.m/master/init.m";
	
	If[newest == $Failed || StringSplit[newest, "\n"][[1]] != "(* ::Package:: *)",
		Return[];
	];
	
	getTimestamp[str_] := (
		m = StringCases[str, StartOfLine ~~ "(* Timestamp: " ~~ Shortest@x__ ~~ " *)" ~~ EndOfLine -> x];
		DateList@m[[1]]
	);
	
	If[DateDifference[getTimestamp@current, getTimestamp@newest] <= 0,
		Return[];
	];
	
	If[StringTrim@current != StringTrim@newest,
		WriteString[f = OpenWrite@initPath, newest];
		Close@f;
		
		CreateDialog[{
			"Mathematica's init.m has been updated!\nRestart Mathematica to apply the changes.",
			DefaultButton[]
		}];
	];
];

Begin["System`"];
SinDeg[d_] := Sin[d * Degree];
CosDeg[d_] := Cos[d * Degree];
TanDeg[d_] := Tan[d * Degree];

ArcSinDeg[d_] := ArcSin[d] / Degree;
ArcCosDeg[d_] := ArcCos[d] / Degree;
ArcTanDeg[d_] := ArcTan[d] / Degree;

CenterDot = Times;

PlusMinus[{a1_, a2_}] := (
	r = {a1, -a2};
	If[r[[1]] == r[[2]], r[[1]], r, r]
);
PlusMinus[a_] := PlusMinus[{a, a}];
PlusMinus[{a1_, a2_}, {b1_, b2_}] := (
	r = {a1 + b1, a2 - b2};
	If[r[[1]] == r[[2]], r[[1]], r, r]
);
PlusMinus[{a1_, a2_}, b_] := PlusMinus[{a1, a2}, {b, b}];
PlusMinus[a_, {b1_, b2_}] := PlusMinus[{a, a}, {b1, b2}];
PlusMinus[a_, b_] := PlusMinus[{a, a}, {b, b}];

MinusPlus[{a1_, a2_}] := (
	r = {-a1, a2};
	If[r[[1]] == r[[2]], r[[1]], r, r]
);
MinusPlus[a_] := MinusPlus[{a, a}];
MinusPlus[{a1_, a2_}, {b1_, b2_}] := (
	r = {a1 - b1, a2 + b2};
	If[r[[1]] == r[[2]], r[[1]], r, r]
);
MinusPlus[{a1_, a2_}, b_] := MinusPlus[{a1, a2}, {b, b}];
MinusPlus[a_, {b1_, b2_}] := MinusPlus[{a, a}, {b1, b2}];
MinusPlus[a_, b_] := MinusPlus[{a, a}, {b, b}];

InfixNotation[ParsedBoxWrapper["\[CirclePlus]"], BitXor];

PlotIntersect[f1_, f2_, o_, options_] := (
	x = o[[1]];
	solution = Solve[y == f1 && y == f2, {x, y}];
	Show[Plot[{f1, f2}, o], ListPlot[Transpose[{x /. solution, y /. solution}], PlotStyle -> {Red, PointSize[0.0125]}], options]
);
	
PlotIntersect[f1_, f2_, o_] := PlotIntersect[f1, f2, o, {}];

PlotDefiniteIntegral[f_, from_, to_, margin_] := Show@{
	Plot[f[x], {x, from - margin, to + margin}, AxesOrigin -> {0, 0}, Epilog -> {
		{Black, Line[{{from, 0}, {from, f[from]}}]},
		{Black, Line[{{to, 0}, {to, f[to]}}]}
	}],
	Plot[f[x], {x, from, to}, Filling -> 0, FillingStyle -> {LightRed, LightGreen}]
};
PlotDefiniteIntegral[f_, from_, to_] := PlotDefiniteIntegral[f, from, to, 0];

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
];

ManToGif[man_, name_String, step_Integer] :=
Export[name <> ".gif",
	Import[
		Export[name <> Which[$OperatingSystem == "MacOSX", ".mov", $OperatingSystem == "Windows", ".avi"],
	 man],"ImageList"][[1 ;; -1 ;; step]]
];

numPlot[ss_,{s_,e_},ee_]:=intPlot[{{ss,{s,e},ee}}];
numPlot[ints:{{_String,{_?NumericQ,_?NumericQ},_String}..}]:=Module[{i=0,c=ColorData[1,"ColorList"]},With[{min=Min[ints[[All,2,1]]],max=Max[ints[[All,2,2]]]},Graphics[Table[With[{ss=int[[1]],s=int[[2,1]],e=int[[2,2]],ee=int[[3]]},{c[[++i+1]],Thickness[.01],Text[Style[ss,Large,c[[i+1]],Bold],{s,i}],Text[Style[ee,Large,c[[i+1]],Bold],{e,i}],Line[{{s,i},{e,i}}]}],{int,ints}],Axes->{True,False},AxesStyle->Directive[Thin,Blue,12],PlotRange->{{0,max+.2 Abs@(min-max)},{0,++i}},AspectRatio->.2]]];

(* Deca is intentionally left out as only one character prefixes are supported *)
$SIPrefixes={"Y"->"Yotta","Z"->"Zetta","E"->"Exa","P"->"Peta","T"->"Tera","G"->"Giga","M"->"Mega","k"->"Kilo","h"->"Hecto","d"->"Deci","c"->"Centi","m"->"Milli","\[Mu]"|"\[Micro]"->"Micro","n"->"Nano","p"->"Pico","f"->"Femto","a"->"Atto","z"->"Zepto","y"->"Yocto"};
$UnitAbbreviations={"\[Degree]"->"angularDegrees","\[Degree]C"->"degreesCelsius","\[CapitalOmega]"->"ohms","A"->"amperes","Bq"->"becquerels","C"->"coulombs","F"->"farads","Gy"->"grays","H"->"henries","Hz"->"hertz","J"->"joules","K"->"kelvins","L"->"liters","M"->"molar","N"->"newtons","Pa"->"pascals","S"->"siemens","Sv"->"sieverts","T"->"teslas","V"->"volts","W"->"watts","Wb"->"webers","a"->"julianYears","atm"->"atmospheres","au"->"astronomicalUnit","bar"->"bars","cd"->"candelas","d"->"days","eV"->"electronvolts","g"->"grams","h"->"hours","kat"->"katals","lm"->"lumens","lx"->"lux","m"->"meters","min"->"minutes","mol"->"moles","rad"->"radians","s"->"seconds","sr"->"steradians"};

FirstDropWhile[list_, cond_] := (
	l = LengthWhile[list,cond];
	If[l == Length[list],
		None,
		list[[l+1]]
	]
);
StringCapitalize[str_] := ToUpperCase @ Characters[str][[1]] <> StringDrop[str, 1];
ReplaceUnit[str_] := str /. $UnitAbbreviations;
ReplaceSIPrefix[str_] := (Characters[str][[1]] /. $SIPrefixes) <> StringDrop[str, 1];

UnitFullName[str_]:=(
	transformations = {Identity, StringCapitalize,
		Composition[StringCapitalize,ReplaceUnit], ReplaceSIPrefix,
		(ReplaceSIPrefix@Characters[#][[1]]) <> ReplaceUnit[StringDrop[#,1]]&
	};
	candidates = Flatten[{#, # <> "s"}& /@ Through[transformations[str]]];
	FirstDropWhile[candidates, !KnownUnitQ@# &]
);

CurrentValue[$FrontEnd, InputAliases] = 
	Append[DeleteCases[CurrentValue[$FrontEnd, InputAliases], "qu" -> _],
	"qu" -> TemplateBox[{"\[SelectionPlaceholder]", "\[Placeholder]"}, 
	"QuantityUnit", Tooltip -> "Unit Template", 
	DisplayFunction -> (PanelBox[RowBox[{#1, StyleBox[#2, "QuantityUnitTraditionalLabel"]}], FrameMargins -> 2] &), 
	InterpretationFunction -> (With[{unit = #2 /. s_String?LetterQ :> "\""~~(UnitFullName[s])~~"\"" /. s_String :> (s /. "\[CenterDot]" -> "*")},
		If[KnownUnitQ@@MakeExpression@unit,
			RowBox[{"Quantity", "[", #1, ",", unit, "]"}],
			RowBox[{"Quantity", "[", #1, ",", "\""~~StringTake[ToString[MakeExpression@#2, InputForm], {14, -2}]~~"\"", "]"}]
		]] &)]];

SolvePolynomialCoordinates[coordinates_] := (length = Length[coordinates];
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
	Solve[equations, variables][[1]]
);
 
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

Format[d[f_, x_], TraditionalForm] := DisplayForm[RowBox[{FractionBox["\[DifferentialD]",
                                                  RowBox[{"\[DifferentialD]", x}]], f}]];

Notation`AutoLoadNotationPalette = False;
Needs["Notation`"];
Symbolize[ParsedBoxWrapper[SubscriptBox["_", "_"]]];
SpecificRules = {d[x_, x_] :> 1, d[(f_)[x_], x_] :> D[f[x], x],
                 d[(a_)^(x_), x_] :> D[a^x, x] /; FreeQ[a, x]};

ConstantRule = d[c_, x_] :> 0 /; FreeQ[c, x];

LinearityRule = {d[f_ + g_, x_] :> d[f, x] + d[g, x],
                 d[c_ f_, x_] :> c d[f, x] /; FreeQ[c, x]};

PowerRule = {d[x_, x_] :> 1, d[(x_)^(a_), x_] :> a*x^(a - 1) /; FreeQ[a, x]};

ProductRule = d[f_ g_, x_] :> d[f, x] g + f d[g, x];

QuotientRule = d[(f_)/(g_), x_] :> (d[f, x]*g - f*d[g, x])/g^2;

InverseFunctionRule := d[InverseFunction[f_][x_], x_] :>
                      1/Derivative[1][f][InverseFunction[f][x]];

ChainRule = {d[(f_)^(a_), x_] :> a*f^(a - 1)*d[f, x] /; FreeQ[a, x],
             d[(a_)^(f_), x_] :> Log[a]*a^f*d[f, x] /; FreeQ[a, x],
             d[(f_)[g_], x_] :> (D[f[x], x] /. x -> g)*d[g, x],
             d[(f_)^(g_), x_] :> f^g*d[g*Log[f], x]};

$RuleNames = {"Specific Rules", "Constant Rule", "Linearity Rule", "Power Rule",
              "Product Rule", "Quotient Rule", "Inverse Function Rule", "Chain Rule"};

displayStart[expr_] := CellPrint[
  Cell[BoxData[MakeBoxes[HoldForm[expr], TraditionalForm]], "Output", 
   Evaluatable -> False, CellMargins -> {{Inherited, Inherited}, {10, 10}}, 
   CellFrame -> False, CellEditDuplicate -> False]];

displayDerivative[expr_, k_Integer] := CellPrint[
  Cell[BoxData[TooltipBox[RowBox[{InterpretationBox["=", Sequence[]], "  ", 
       MakeBoxes[HoldForm[expr], TraditionalForm]}], $RuleNames[[k]], 
     LabelStyle -> "TextStyling"]], "Output", Evaluatable -> False, 
   CellMargins -> {{Inherited, Inherited}, {10, 10}}, 
   CellFrame -> False, CellEditDuplicate -> False]];

WalkD[f_, x_] := Module[{derivative, oldderivative, k}, 
        derivative = d[f, x]; displayStart[derivative];
        While[! FreeQ[derivative, d],
            oldderivative = derivative; k = 0;
            While[oldderivative == derivative,
                      k++;
                      derivative = derivative /. 
                              ToExpression[StringReplace[$RuleNames[[k]], " " -> ""]]];
            displayDerivative[derivative, k]];
        D[f, x]];

End[];
