(* ::Package:: *)
(* Timestamp: 2014-02-27 15:10 *)

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
		Epilog -> {PointSize[Medium], Point[data]},
		Evaluate @ otherOptions
	]
];

fitPlot[data_, expr_, pars_, vars_] := fitPlot[data, expr, pars, vars, Sequence@{}];

plot3DCrossSection[eq_, param1_, param2_] := Module[{a, b}, 
	Manipulate[
		GraphicsRow[{
			Plot3D[eq, Evaluate@param1, Evaluate@param2, Mesh -> {{a}, {b}}],
			Plot[eq /. param2[[1]] -> b, Evaluate@param1, AxesLabel -> {"x", "z"}],
			Plot[eq /. param1[[1]] -> a, Evaluate@param2, AxesLabel -> {"y", "z"}]
		}, ImageSize -> Full],
	{{a, Mean[param1[[2 ;; 3]]], "x"}, param1[[2]], param1[[3]]},
	{{b, Mean[param2[[2 ;; 3]]], "y"}, param2[[2]], param2[[3]]}]	
];

removeSubscript[s_String] :=
	StringReplace[s,
		"\!\(\*SubscriptBox[\(" ~~ Shortest[x__] ~~ "\), \(" ~~ Shortest[y__] ~~ "\)]\)" :> x <> y
	];
removeSubscript[x_] := x;

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

thermodynamicData = {{"Ag(s)", 0, 42.55, 0}, {"Ag+(aq)", 105.79, 73.45, 77.16}, 
 {"AgCl(s)", -127.01, 96.25, -109.86}, {"AgI(s)", -61.87, 115.83, -66.22}, 
 {"Ba(OH)2\[CenterDot]H2O(s)", -3342.2, 427, -2792.2}, {"BaCl2\[CenterDot]2H2O(s)", -1456.9, 202., 
  -1293.2}, {"C(s,grafit)", 0, 5.74, 0}, {"C(s,diamant)", 1.895, 2.377, 2.832}, 
 {"CH4(g)", -74.6, 186.3, -50.5}, {"C2H4(g)", 52.3, 219.3, 68.27}, 
 {"C7H16(l)", -224.2, 326.1, 1.89}, {"CH3OH(l)", -239.2, 126.8, -166.6}, 
 {"CH3CH2OH(l)", -277.6, 160.7, -174.8}, {"CO(g)", -110.53, 197.66, -137.25}, 
 {"CO2(g)", -393.51, 213.785, -394.4}, {"CaCO3(s)", -1206.92, 92.9, -1128.84}, 
 {"CaO(s)", -634.92, 38.1, -603.3}, {"Cl-(aq)", -167.08, 56.6, -131.2}, 
 {"CrO4--(aq)", -881.15, 50.2, -727.8}, {"H2(g)", 0, 130.68, 0}, 
 {"H+(aq)", 0, 0, 0}, {"H3O+(aq)", -285.83, 69.95, -237.13}, 
 {"H2O(l)", -285.83, 69.95, -237.13}, {"H2O(g)", -241.8, 188.8, -228.6}, 
 {"HCl(g)", -92.3, 186.9, -95.3}, {"HI(g)", 25.94, 206.44, 1.32}, 
 {"I2(s)", 0, 116.14, 0}, {"I-(aq)", -56.78, 106.45, -51.6}, 
 {"N2(g)", 0, 191.61, 0}, {"NH3(g)", -45.5, 192.8, -16.4}, 
 {"NH4-(aq)", -132.4, 113.4, -79.3}, {"NH4Cl(s)", -314.6, 94.6, -202.9}, 
 {"NH4NO3(s)", -365.3, 151.1, -183.9}, {"NO(g)", 91.2, 210.8, 87.6}, {"NO2(g)", 33.1, 240.1, 51.3},
 {"N2O4(g)", 11.4, 304.3, 99.8}, {"NO3-(aq)", -207, 146.4, -111.3}, 
 {"Na(s)", 0, 51.3, 0}, {"Na+(aq)", -240.34, 58.45, -261.9}, 
 {"NaCl(s)", -411.2, 72.1, -384.1}, {"O2(g)", 0, 205.15, 0}, 
 {"OH-(aq)", -230.01, -10.9, -157.2}, {"SO2(g)", -296.81, 248.23, -300.1}, 
 {"SO3(g)", -395.72, 256.83, -371.03}};

deltaThermo[reactants_ -> products_, i_Integer, rule_List] := 
  Quantity[(products /. rule) - (reactants /. rule), 
   Evaluate@
    If[i == 2, "Joules"/("Moles"*"Kelvins"), "Kilojoules"/"Moles"]];
deltaThermo[r_Rule, i_Integer] := 
  deltaThermo[r, i, Rule @@@ thermodynamicData[[All, {1, i + 1}]]];
deltaH[r_Rule] := deltaThermo[r, 1];
deltaS[r_Rule] := deltaThermo[r, 2];
deltaG[r_Rule] := deltaThermo[r, 3];
deltaH[r_Rule, otherRule_] := deltaThermo[r, 1, otherRule];
deltaS[r_Rule, otherRule_] := deltaThermo[r, 2, otherRule];
deltaG[r_Rule, otherRule_] := deltaThermo[r, 3, otherRule];

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
