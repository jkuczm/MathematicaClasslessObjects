(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`WithOrdinaryObjectSet`", {"MUnit`"}]


Get["ClasslessObjects`"]


AppendTo[$ContextPath, "ClasslessObjects`Private`"]


declareMockObjectWithAlteredSetUpValues[obj_] := (
	ObjectQ[obj] ^= True;
	obj /: Set[obj[_], _] = "altered Set";
	obj /: SetDelayed[obj[_], _] = "altered SetDelayed";
	obj /: Unset[obj[_]] = "altered Unset";
)



(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


TestMatch[
	WithOrdinaryObjectSet[]
	,
	HoldPattern @ WithOrdinaryObjectSet[]
	,
	Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 0, 2]
	,
	TestID -> "no args: evaluation"
]


Module[
	{obj, oldUpValues, oldAttributes}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	TestMatch[
		WithOrdinaryObjectSet[obj]
		,
		HoldPattern @ WithOrdinaryObjectSet[obj]
		,
		Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 1, 2]
		,
		TestID -> "1 arg: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "1 arg: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "1 arg: after: Attributes"
	];
]


TestMatch[
	WithOrdinaryObjectSet[arg1, arg2]
	,
	HoldPattern @ WithOrdinaryObjectSet[arg1, arg2]
	,
	Message[Object::object, 1, WithOrdinaryObjectSet[arg1, arg2]]
	,
	TestID -> "2 args: first non-object: evaluation"
];


Module[
	{obj, oldUpValues, oldAttributes}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	TestMatch[
		WithOrdinaryObjectSet[obj, arg2, arg3]
		,
		HoldPattern @ WithOrdinaryObjectSet[obj, arg2, arg3]
		,
		Message[WithOrdinaryObjectSet::argrx, WithOrdinaryObjectSet, 3, 2]
		,
		TestID -> "3 args: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "3 args: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "3 args: after: Attributes"
	];
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsubsection:: *)
(*Non-protected object*)


Module[
	{
		obj, evaluationResult,
		oldUpValues, oldAttributes,
		upValuesInside, attributesInside
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			upValuesInside = UpValues[obj];
			attributesInside = Attributes[obj];
			
			UpValues[obj] = {HoldPattern[something[obj]] :> "some value"};
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "non-protected: evaluation"
	];
	
	Test[
		upValuesInside
		,
		{HoldPattern[ObjectQ[obj]] :> True}
		,
		TestID -> "non-protected: inside: UpValues"
	];
	Test[
		attributesInside
		,
		oldAttributes
		,
		TestID -> "non-protected: inside: Attributes"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "non-protected: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "non-protected: after: Attributes"
	];
]


(* ::Subsubsection:: *)
(*Protected object*)


Module[
	{
		obj, evaluationResult,
		oldUpValues, oldAttributes,
		upValuesInside, attributesInside
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	Protect[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			upValuesInside = UpValues[obj];
			attributesInside = Attributes[obj];
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "protected: evaluation"
	];
	
	Test[
		upValuesInside
		,
		{HoldPattern[ObjectQ[obj]] :> True}
		,
		TestID -> "protected: inside: UpValues"
	];
	Test[
		attributesInside
		,
		oldAttributes
		,
		TestID -> "protected: inside: Attributes"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "protected: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "protected: after: Attributes"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
