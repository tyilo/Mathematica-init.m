(* ::Package:: *)
(* Timestamp: 2014-02-17 01:10 *)

(** User Mathematica initialization file **)

(* Needed to make function appear in the auto-complete prompt *)
Begin["System`"];

(* Disable WolframAlpha *)
Unprotect[WolframAlpha];
ClearAll[WolframAlpha];
WolframAlpha[x__] := Null;
Protect[WolframAlpha];

(* My definitions *)

updateInitFile::networkError = "Failed to retrieve the latest init.m version from github.";
updateInitFile[] := (
	initPath = ToFileName[{$UserBaseDirectory, "Kernel"}, "init.m"];
	current = StringJoin@ReadList[f = OpenRead@initPath, Character];
	Close@f;
	newest = URLFetch@"https://raw.github.com/Tyilo/Mathematica-init.m/master/init.m";
	
	If[newest == $Failed || StringSplit[newest, "\n"][[1]] != "(* ::Package:: *)",
		Message[updateInitFile::networkError];
		Return[];
	];
	
	(* URLFetch truncates the last newline *)
	newest = newest <> "\n";
	
	getTimestamp[str_] := (
		m = StringCases[str, StartOfLine ~~ "(* Timestamp: " ~~ Shortest@x__ ~~ " *)" ~~ EndOfLine -> x];
		DateList@m[[1]]
	);
	
	If[DateDifference[getTimestamp@current, getTimestamp@newest] <= 0,
		Return["Your init.m is already at the latest version."];
	];
	
	WriteString[f = OpenWrite@initPath, newest];
	Close@f;
	
	"Mathematica's init.m has been updated!\nRestart Mathematica to apply the changes."
);

fixMathematica[] := DeleteFile @ FileNameJoin[{$UserBaseDirectory, "FrontEnd", "init.m"}];

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

allProperties[f_, elem_] := TableForm[{#, f[elem, #]} & /@ f["Properties"]];

plotIntersect[f1_, f2_, o_, options_] := (
	x = o[[1]];
	solution = Solve[y == f1 && y == f2, {x, y}];
	Show[Plot[{f1, f2}, o], ListPlot[Transpose[{x /. solution, y /. solution}], PlotStyle -> {Red, PointSize[0.0125]}], options]
);
	
plotIntersect[f1_, f2_, o_] := plotIntersect[f1, f2, o, {}];

plotDefiniteIntegral[f_, from_, to_, margin_] := Show@{
	Plot[f[x], {x, from - margin, to + margin}, AxesOrigin -> {0, 0}, Epilog -> {
		{Black, Line[{{from, 0}, {from, f[from]}}]},
		{Black, Line[{{to, 0}, {to, f[to]}}]}
	}],
	Plot[f[x], {x, from, to}, Filling -> 0, FillingStyle -> {LightRed, LightGreen}]
};
plotDefiniteIntegral[f_, from_, to_] := plotDefiniteIntegral[f, from, to, 0];

fitPlotOptions = {showFunction, showParams, showRSquared};
Options[fitPlot] = Table[x -> True, {x, fitPlotOptions}] ~Join~ Options[Plot];

fitPlot[data_, expr_, pars_, vars_, options:OptionsPattern[]] := Block[{fit, params, col1, plotRange, xmin, xmax, labelOptions, labels, R, otherOptions, fitted},
	Assert[Length@Dimensions[data] >= 2 && Dimensions[data][[2]] == 2];
	Assert[Head@vars == Symbol];
	
	fit = NonlinearModelFit[data, expr, pars, vars];
	params = fit["BestFitParameters"];
	col1 = data[[All,1]];
	plotRange = OptionValue[PlotRange];
	{xmin, xmax} = If[Head@plotRange == List && Length@plotRange == 2 && Length@plotRange[[1]] == 2,
		plotRange[[1]],
		{Automatic, Automatic}
	];
	xmin = xmin /. Automatic -> Min[col1];
	xmax = xmax /. Automatic -> Max[col1];
	labelOptions = fitPlotOptions;
	labels = Flatten@Position[OptionValue[#] & /@ labelOptions, True] /. {
		1 -> Normal[fit],
		2 -> params,
		3 -> R^2 == fit["RSquared"]
	};
	otherOptions = Sequence @@ DeleteCases[{options}, Alternatives @@ fitPlotOptions -> _];
	Plot[fit[vars], {vars, xmin, xmax},
		PlotLabel -> Column@labels,
		AxesLabel -> {vars},
		Epilog -> {PointSize[Medium], Point[data]},
		Evaluate @ otherOptions
	]
];

fitPlot[data_, expr_, pars_, vars_] := fitPlot[data, expr, pars, vars, Sequence@{}];

removeSubscript[s_String] :=
	StringReplace[s,
		"\!\(\*SubscriptBox[\(" ~~ Shortest[x__] ~~ "\), \(" ~~ Shortest[y__] ~~ "\)]\)" :> x <> y
	];

molecularWeight[s_String] :=
	ToExpression @ StringReplace[
		StringReplace[
			StringReplace[removeSubscript @ s,
				x:RegularExpression["[A-Z][a-z]*"] :>
				"ElementData[\"" <> x <> "\",\"AtomicWeight\"]+"
			],
			x:DigitCharacter .. :> "*" <> x <> "+"],
		{"+*" -> "*", "+" ~~ EndOfString -> "", "+)" -> ")"}];

chemicalTable[formula_] := Module[{chemicals, properties},
	chemicals = Check[ChemicalData[removeSubscript @ formula, "StandardName"], Break[]];
	chemicals = If[Length[chemicals] == 0, {chemicals}, chemicals];
	properties = {"StandardName", "MolecularFormulaDisplay", "StructureDiagram"};
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

(* Deca is intentionally left out as only one character prefixes are supported *)
$mySIPrefixes={"Y"->"Yotta","Z"->"Zetta","E"->"Exa","P"->"Peta","T"->"Tera","G"->"Giga","M"->"Mega","k"->"Kilo","h"->"Hecto","d"->"Deci","c"->"Centi","m"->"Milli","\[Mu]"|"\[Micro]"->"Micro","n"->"Nano","p"->"Pico","f"->"Femto","a"->"Atto","z"->"Zepto","y"->"Yocto"};
$unitAbbreviations={"\[Degree]"->"angularDegrees","\[Degree]C"|"℃"->"degreesCelsius","\[Degree]F"|"℉"->"degreesFahrenheit","\[CapitalOmega]"->"ohms","A"->"amperes","Bq"->"becquerels","C"->"coulombs","Da"|"u"->"daltons","F"->"farads","Gy"->"grays","H"->"henries","Hz"->"hertz","J"->"joules","K"->"kelvins","L"->"liters","ly"->"lightYears","M"->"molar","N"->"newtons","Pa"->"pascals","pc"->"parsecs","S"->"siemens","Sv"->"sieverts","T"->"teslas","V"->"volts","W"->"watts","Wb"->"webers","a"->"julianYears","atm"->"atmospheres","au"->"astronomicalUnit","bar"->"bars","cd"->"candelas","d"->"days","eV"->"electronvolts","g"->"grams","h"->"hours","kat"->"katals","lm"->"lumens","lx"->"lux","m"->"meters","min"->"minutes","mol"->"moles","rad"->"radians","s"->"seconds","sr"->"steradians"};
$constantAbbreviations={"\[CurlyEpsilon]0"|"\[Epsilon]0"->"electricConstant","\[Mu]0"->"magneticConstant","\[Sigma]"->"stefanBoltzmannConstant","c"->"speedOfLight","e"->"elementaryCharge","G"->"gravitationalConstant","h"->"planckConstant","k"->"boltzmannConstant","me"->"electronMass","NA"->"avogadroConstant","R"->"molarGasConstant"};

firstDropWhile[list_, cond_] := (
	l = LengthWhile[list, cond];
	If[l == Length[list],
		Null,
		list[[l+1]]
	]
);
stringCapitalize[str_String] := ToUpperCase @ Characters[str][[1]] <> StringDrop[str, 1];
replaceSIPrefix[str_String] := (Characters[str][[1]] /. $mySIPrefixes) <> StringDrop[str, 1];

fullName[str_String, rule_] := Module[{applyRule, transformations, candidates},
	applyRule = (# /. rule)&;
	transformations = {Identity, stringCapitalize,
		Composition[stringCapitalize, applyRule], replaceSIPrefix,
		(replaceSIPrefix@Characters[#][[1]]) <> applyRule[StringDrop[#,1]]&
	};
	candidates = Flatten[{#, # <> "s"}& /@ Through[transformations[str]]];
	firstDropWhile[candidates, !KnownUnitQ@# &]
];

unitFullName[str_String] := fullName[str, $unitAbbreviations];
knownUnitAbbreviationQ[str_String] := (
	unitFullName[str] =!= Null
);
constantFullName[str_String] := fullName[str, $constantAbbreviations];
knownConstantAbbreviationQ[str_String] := (
	constantFullName[str] =!= Null
);

fullUnit[u_] := Module[{}, 
	Hold @ Evaluate[u /. {s_String?LetterQ :> unitFullName[s], CenterDot -> Times}]
];

(* Next line required for Unprotect to work, see http://stackoverflow.com/a/5649618/640584 *)
(*
{Block, Hold, Evaluate, FullUnit, If, KnownUnitQ, Quantity, UnitConvert, ReleaseHold, TrueQ};
Unprotect[Quantity, UnitConvert];
Quantity[n_, u_] := Block[{$inQuantity = True, fu},
	fu = fullUnit[u];
	If[KnownUnitQ @@ fu,
		Quantity[n, ReleaseHold @ fu],
		Quantity[n, u]
	]] /; !TrueQ[$inQuantity] (* && !KnownUnitQ[u] *)
UnitConvert[n_, u_] := Block[{$inUnitConvert = True, fu},
	fu = fullUnit[u];
	If[KnownUnitQ @@ fu,
		Quiet @ UnitConvert[n, ReleaseHold @ fu],
		UnitConvert[n, u]
	]] /; !TrueQ[$inUnitConvert] (* && !KnownUnitQ[u] *)
Protect[Quantity, UnitConvert];
*)

CurrentValue[$FrontEnd, InputAliases] = DeleteCases[CurrentValue[$FrontEnd, InputAliases], "qu"|"const" -> _];

CurrentValue[$FrontEnd, InputAliases] = Join[CurrentValue[$FrontEnd, InputAliases], {
	"qu" -> TemplateBox[{"\[SelectionPlaceholder]", "\[Placeholder]"}, 
		"QuantityUnit", Tooltip -> "Unit Template", 
		DisplayFunction -> (PanelBox[RowBox[{#1, StyleBox[#2, "QuantityUnitTraditionalLabel"]}], FrameMargins -> 2] &), 
		InterpretationFunction -> (With[{unit = #2 /. s_String?knownUnitAbbreviationQ :> "\""~~(unitFullName[s])~~"\"" /. s_String :> (s /. "\[CenterDot]" -> "*")},
			(*Print[unit];*)
			If[KnownUnitQ@@MakeExpression@unit,
				RowBox[{"Quantity", "[", #1, ",", unit, "]"}],
				RowBox[{"Quantity", "[", #1, ",", "\""~~StringTake[ToString[MakeExpression@#2, InputForm], {14, -2}]~~"\"", "]"}]
			]
		] &)
	],
	"const" -> TemplateBox[{"\[SelectionPlaceholder]"}, 
		"Constant", Tooltip -> "Constant Template", 
		DisplayFunction -> (PanelBox[RowBox[{StyleBox[#, "QuantityUnitTraditionalLabel"]}], FrameMargins -> 2] &), 
		InterpretationFunction -> (With[{const = # /. s_String?knownConstantAbbreviationQ :> "\""~~(constantFullName[s])~~"\""},
			(*Print[const];*)
			If[KnownUnitQ@@MakeExpression@const,
				RowBox[{"Quantity", "[", 1, ",", const, "]"}],
				RowBox[{"Quantity", "[", 1, ",", "\""~~StringTake[ToString[MakeExpression@#, InputForm], {14, -2}]~~"\"", "]"}]
			]
		] &)
	]
}];

solvePolynomialCoordinates[coordinates_] := (length = Length[coordinates];
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

(* Borrowed definitions *)

manToGif[man_, name_String, step_Integer] :=
Export[name <> ".gif",
	Import[
		Export[name <> Which[$OperatingSystem == "MacOSX", ".mov", $OperatingSystem == "Windows", ".avi"],
	 man],"ImageList"][[1 ;; -1 ;; step]]
];

numPlot[ss_,{s_,e_},ee_]:=intPlot[{{ss,{s,e},ee}}];
numPlot[ints:{{_String,{_?NumericQ,_?NumericQ},_String}..}]:=Module[{i=0,c=ColorData[1,"ColorList"]},With[{min=Min[ints[[All,2,1]]],max=Max[ints[[All,2,2]]]},Graphics[Table[With[{ss=int[[1]],s=int[[2,1]],e=int[[2,2]],ee=int[[3]]},{c[[++i+1]],Thickness[.01],Text[Style[ss,Large,c[[i+1]],Bold],{s,i}],Text[Style[ee,Large,c[[i+1]],Bold],{e,i}],Line[{{s,i},{e,i}}]}],{int,ints}],Axes->{True,False},AxesStyle->Directive[Thin,Blue,12],PlotRange->{{0,max+.2 Abs@(min-max)},{0,++i}},AspectRatio->.2]]];
 
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

specificRules = {d[x_, x_] :> 1, d[(f_)[x_], x_] :> D[f[x], x],
                 d[(a_)^(x_), x_] :> D[a^x, x] /; FreeQ[a, x]};

constantRule = d[c_, x_] :> 0 /; FreeQ[c, x];

linearityRule = {d[f_ + g_, x_] :> d[f, x] + d[g, x],
                 d[c_ f_, x_] :> c d[f, x] /; FreeQ[c, x]};

powerRule = {d[x_, x_] :> 1, d[(x_)^(a_), x_] :> a*x^(a - 1) /; FreeQ[a, x]};

productRule = d[f_ g_, x_] :> d[f, x] g + f d[g, x];

quotientRule = d[(f_)/(g_), x_] :> (d[f, x]*g - f*d[g, x])/g^2;

inverseFunctionRule := d[InverseFunction[f_][x_], x_] :>
                      1/Derivative[1][f][InverseFunction[f][x]];

chainRule = {d[(f_)^(a_), x_] :> a*f^(a - 1)*d[f, x] /; FreeQ[a, x],
             d[(a_)^(f_), x_] :> Log[a]*a^f*d[f, x] /; FreeQ[a, x],
             d[(f_)[g_], x_] :> (D[f[x], x] /. x -> g)*d[g, x],
             d[(f_)^(g_), x_] :> f^g*d[g*Log[f], x]};

$dRuleNames = {"Specific Rules", "Constant Rule", "Linearity Rule", "Power Rule",
              "Product Rule", "Quotient Rule", "Inverse Function Rule", "Chain Rule"};

displayStart[expr_] := CellPrint[
  Cell[BoxData[MakeBoxes[HoldForm[expr], TraditionalForm]], "Output", 
   Evaluatable -> False, CellMargins -> {{Inherited, Inherited}, {10, 10}}, 
   CellFrame -> False, CellEditDuplicate -> False]];

displayDerivative[expr_, k_Integer] := CellPrint[
  Cell[BoxData[TooltipBox[RowBox[{InterpretationBox["=", Sequence[]], "  ", 
       MakeBoxes[HoldForm[expr], TraditionalForm]}], $dRuleNames[[k]], 
     LabelStyle -> "TextStyling"]], "Output", Evaluatable -> False, 
   CellMargins -> {{Inherited, Inherited}, {10, 10}}, 
   CellFrame -> False, CellEditDuplicate -> False]];

walkD[f_, x_] := Module[{derivative, oldderivative, k}, 
        derivative = d[f, x]; displayStart[derivative];
        While[! FreeQ[derivative, d],
            oldderivative = derivative; k = 0;
            While[oldderivative == derivative,
                      k++;
                      derivative = derivative /. 
                              ToExpression[StringReplace[$dRuleNames[[k]], {" " -> "", StartOfString ~~ c:_ -> ToLowerCase[c]}]]];
            displayDerivative[derivative, k]];
        D[f, x]];

End[];

(*
  Make it possible to work with subscripted and overscripted variables.
  This needs to stay after the End call or it will fuck up the [esc]qu[esc] function.
*)
Notation`AutoLoadNotationPalette = False;
Needs["Notation`"];
Symbolize[ParsedBoxWrapper[SubscriptBox["_", "_"]]];
Symbolize[ParsedBoxWrapper[OverscriptBox["_","_"]]];
