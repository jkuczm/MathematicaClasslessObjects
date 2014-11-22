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


mockObjectUpValuesPattern[obj_] := {
	Verbatim[RuleDelayed][
		Verbatim[HoldPattern][HoldPattern[ObjectQ[obj]]],
		True
	],
	Verbatim[RuleDelayed][
		Verbatim[HoldPattern][wrapper_Symbol[HoldPattern[obj[_] = _]]],
		"altered Set"
	],
	Verbatim[RuleDelayed][
		Verbatim[HoldPattern][wrapper_Symbol[HoldPattern[obj[_] := _]]],
		"altered SetDelayed"
	],
	Verbatim[RuleDelayed][
		Verbatim[HoldPattern][wrapper_Symbol[HoldPattern[obj[_] =. ]]],
		"altered Unset"
	]
}



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
		Message[WithOrdinaryObjectSet::argr, WithOrdinaryObjectSet, 2]
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
		upValuesInside, attributesInside,
		accessor, value
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			upValuesInside = UpValues[obj];
			attributesInside = Attributes[obj];
			
			accessor[obj] ^= value;
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "non-protected: evaluation"
	];
	
	TestMatch[
		upValuesInside
		,
		mockObjectUpValuesPattern[obj]
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
		Prepend[oldUpValues, HoldPattern[accessor[obj]] :> value]
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
		accessor1, accessor2,
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
			
			accessor1[obj1] ^= value1;
			accessor2[obj2] ^= value2;
			
			evaluationResult
		]
		,
		evaluationResult
		,
		TestID -> "non-protected: list: evaluation"
	];
	
	TestMatch[
		upValuesInside1
		,
		mockObjectUpValuesPattern[obj1]
		,
		TestID -> "non-protected: list: inside: UpValues: first object"
	];
	TestMatch[
		upValuesInside2
		,
		mockObjectUpValuesPattern[obj2]
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
		Prepend[oldUpValues1, HoldPattern[accessor1[obj1]] :> value1]
		,
		TestID -> "non-protected: list: after: UpValues: first object"
	];
	Test[
		UpValues[obj2]
		,
		Prepend[oldUpValues2, HoldPattern[accessor2[obj2]] :> value2]
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
	
	TestMatch[
		upValuesInside
		,
		mockObjectUpValuesPattern[obj]
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
	
	TestMatch[
		upValuesInside1
		,
		mockObjectUpValuesPattern[obj1]
		,
		TestID -> "protected: list: inside: UpValues: first object"
	];
	TestMatch[
		upValuesInside2
		,
		mockObjectUpValuesPattern[obj2]
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


(* ::Subsubsection:: *)
(*Flow Control*)


Module[
	{
		obj, evaluationResult, aborted,
		oldUpValues, oldAttributes,
		accessor, value
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		CheckAbort[
			WithOrdinaryObjectSet[obj,
				accessor[obj] ^= value;
				
				Abort[];
				
				evaluationResult
			]
			,
			aborted
		]
		,
		aborted
		,
		TestID -> "flow control: abort: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		Prepend[oldUpValues, HoldPattern[accessor[obj]] :> value]
		,
		TestID -> "flow control: abort: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "flow control: abort: after: Attributes"
	];
]


Module[
	{
		obj, evaluationResult, thrownValue,
		oldUpValues, oldAttributes,
		accessor, value
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		Catch[
			WithOrdinaryObjectSet[obj,
				accessor[obj] ^= value;
				
				Throw[thrownValue];
				
				evaluationResult
			]
		]
		,
		thrownValue
		,
		TestID -> "flow control: throw no tag: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		Prepend[oldUpValues, HoldPattern[accessor[obj]] :> value]
		,
		TestID -> "flow control: throw no tag: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "flow control: throw no tag: after: Attributes"
	];
]


Module[
	{
		obj, evaluationResult, thrownValue, throwTag,
		oldUpValues, oldAttributes,
		accessor, value
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		Catch[
			WithOrdinaryObjectSet[obj,
				accessor[obj] ^= value;
				
				Throw[thrownValue, throwTag];
				
				evaluationResult
			]
			,
			throwTag
		]
		,
		thrownValue
		,
		TestID -> "flow control: throw with tag: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		Prepend[oldUpValues, HoldPattern[accessor[obj]] :> value]
		,
		TestID -> "flow control: throw with tag: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "flow control: throw with tag: after: Attributes"
	];
]


Module[
	{
		obj, evaluationResult,
		label, lebeledReturnValue,
		oldUpValues, oldAttributes,
		accessor, value
	}
	,
	declareMockObjectWithAlteredSetUpValues[obj];
	
	oldUpValues = UpValues[obj];
	oldAttributes = Attributes[obj];
	
	Test[
		WithOrdinaryObjectSet[obj,
			accessor[obj] ^= value;
			
			Goto[label];
			
			evaluationResult
		];
		Label[label];
		lebeledReturnValue
		,
		lebeledReturnValue
		,
		TestID -> "flow control: goto: evaluation"
	];
	
	Test[
		UpValues[obj]
		,
		Prepend[oldUpValues, HoldPattern[accessor[obj]] :> value]
		,
		TestID -> "flow control: goto: after: UpValues"
	];
	Test[
		Attributes[obj]
		,
		oldAttributes
		,
		TestID -> "flow control: goto: after: Attributes"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
