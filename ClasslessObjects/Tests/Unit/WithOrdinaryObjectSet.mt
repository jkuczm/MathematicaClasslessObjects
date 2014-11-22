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


Module[
	{arg1, arg2}
	,
	TestMatch[
		WithOrdinaryObjectSet[arg1, arg2]
		,
		HoldPattern @ WithOrdinaryObjectSet[arg1, arg2]
		,
		Message[Object::objects, 1, WithOrdinaryObjectSet[arg1, arg2], arg1]
		,
		TestID -> "2 args: first non-object: evaluation"
	]
]

Module[
	{arg2}
	,
	TestMatch[
		WithOrdinaryObjectSet[{}, arg2]
		,
		HoldPattern @ WithOrdinaryObjectSet[{}, arg2]
		,
		Message[Object::objects, 1, WithOrdinaryObjectSet[{}, arg2], {}]
		,
		TestID -> "2 args: first empty list: evaluation"
	]
]

Module[
	{nonObj1, arg2}
	,
	TestMatch[
		WithOrdinaryObjectSet[{nonObj1}, arg2]
		,
		HoldPattern @ WithOrdinaryObjectSet[{nonObj1}, arg2]
		,
		Message[Object::objects,
			1, WithOrdinaryObjectSet[{nonObj1}, arg2], {nonObj1}
		]
		,
		TestID -> "2 args: first list with non-obj: evaluation"
	]
]

Module[
	{nonObj1, nonObj2, arg2}
	,
	TestMatch[
		WithOrdinaryObjectSet[{nonObj1, nonObj2}, arg2]
		,
		HoldPattern @ WithOrdinaryObjectSet[{nonObj1, nonObj2}, arg2]
		,
		Message[Object::objects,
			1,
			WithOrdinaryObjectSet[{nonObj1, nonObj2}, arg2],
			{nonObj1, nonObj2}
		]
		,
		TestID -> "2 args: first list with 2 non-objs: evaluation"
	]
]

Module[
	{obj, oldUpValues, oldAttributes}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	TestMatch[
		WithOrdinaryObjectSet[{obj, nonObj2}, arg2]
		,
		HoldPattern @ WithOrdinaryObjectSet[{obj, nonObj2}, arg2]
		,
		Message[Object::objects,
			1, WithOrdinaryObjectSet[{obj, nonObj2}, arg2], {nonObj2}
		]
		,
		TestID -> "2 args: first list with obj and non-obj: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		oldUpValues
		,
		TestID -> "2 args: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "2 args: after: Attributes"
	];
]


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


Module[
	{
		evaluationResult,
		obj1, obj2,
		oldUpValues1, oldUpValues2, oldAttributes1, oldAttributes2,
		upValuesInside1, upValuesInside2, attributesInside1, attributesInside2,
		something1, something2,
		value1, value2
	}
	,
	declareMockObjectWithAlteredSetUpValues /@ {obj1, obj2};
	
	{oldUpValues1, oldUpValues2} = UpValues /@ {obj1, obj2};
	{oldAttributes1, oldAttributes2} = Attributes /@ {obj1, obj2};
	
	Test[
		WithOrdinaryObjectSet[{obj1, obj2},
			{upValuesInside1, upValuesInside2} = UpValues /@ {obj1, obj2};
			{attributesInside1, attributesInside2} =
				Attributes /@ {obj1, obj2};
			
			UpValues[obj1] = {HoldPattern[something1[obj1]] :> value1};
			UpValues[obj2] = {HoldPattern[something2[obj2]] :> value2};
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "non-protected: list: evaluation"
	];
	
	Test[
		upValuesInside1
		,
		{HoldPattern[ObjectQ[obj1]] :> True}
		,
		TestID -> "non-protected: list: inside: UpValues: first object"
	];
	Test[
		upValuesInside2
		,
		{HoldPattern[ObjectQ[obj2]] :> True}
		,
		TestID -> "non-protected: list: inside: UpValues: second object"
	];
	
	Test[
		attributesInside1
		,
		oldAttributes1
		,
		TestID -> "non-protected: list: inside: Attributes: first object"
	];
	Test[
		attributesInside2
		,
		oldAttributes2
		,
		TestID -> "non-protected: list: inside: Attributes: second object"
	];
	
	
	Test[
		UpValues[obj1]
		,
		oldUpValues1
		,
		TestID -> "non-protected: list: after: UpValues: first object"
	];
	Test[
		UpValues[obj2]
		,
		oldUpValues2
		,
		TestID -> "non-protected: list: after: UpValues: second object"
	];
	
	Test[
		Attributes[obj1]
		,
		oldAttributes1
		,
		TestID -> "non-protected: list: after: Attributes: first object"
	];
	Test[
		Attributes[obj1]
		,
		oldAttributes2
		,
		TestID -> "non-protected: list: after: Attributes: second object"
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


Module[
	{
		evaluationResult,
		obj1, obj2,
		oldUpValues1, oldUpValues2, oldAttributes1, oldAttributes2,
		upValuesInside1, upValuesInside2, attributesInside1, attributesInside2
	}
	,
	declareMockObjectWithAlteredSetUpValues /@ {obj1, obj2};
	
	{oldUpValues1, oldUpValues2} = UpValues /@ {obj1, obj2};
	{oldAttributes1, oldAttributes2} = Attributes /@ {obj1, obj2};
	
	Test[
		WithOrdinaryObjectSet[{obj1, obj2},
			{upValuesInside1, upValuesInside2} = UpValues /@ {obj1, obj2};
			{attributesInside1, attributesInside2} =
				Attributes /@ {obj1, obj2};
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "protected: list: evaluation"
	];
	
	Test[
		upValuesInside1
		,
		{HoldPattern[ObjectQ[obj1]] :> True}
		,
		TestID -> "protected: list: inside: UpValues: first object"
	];
	Test[
		upValuesInside2
		,
		{HoldPattern[ObjectQ[obj2]] :> True}
		,
		TestID -> "protected: list: inside: UpValues: second object"
	];
	
	Test[
		attributesInside1
		,
		oldAttributes1
		,
		TestID -> "protected: list: inside: Attributes: first object"
	];
	Test[
		attributesInside2
		,
		oldAttributes2
		,
		TestID -> "protected: list: inside: Attributes: second object"
	];
	
	
	Test[
		UpValues[obj1]
		,
		oldUpValues1
		,
		TestID -> "protected: list: after: UpValues: first object"
	];
	Test[
		UpValues[obj2]
		,
		oldUpValues2
		,
		TestID -> "protected: list: after: UpValues: second object"
	];
	
	Test[
		Attributes[obj1]
		,
		oldAttributes1
		,
		TestID -> "protected: list: after: Attributes: first object"
	];
	Test[
		Attributes[obj1]
		,
		oldAttributes2
		,
		TestID -> "protected: list: after: Attributes: second object"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
