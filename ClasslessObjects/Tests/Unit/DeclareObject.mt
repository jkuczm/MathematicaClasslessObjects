(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["ClasslessObjects`Tests`Unit`DeclareObject`", {"MUnit`"}]


Get["ClasslessObjects`"]
Get["ClasslessObjects`Tests`Unit`Utilities`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Wrong argument patterns*)


(* ::Subsection:: *)
(*No arguments*)


TestMatch[
	DeclareObject[]
	,
	HoldPattern @ DeclareObject[]
	,
	Message[DeclareObject::argt, DeclareObject, 0, 1, 2]
	,
	TestID -> "no args: DeclareObject evaluation"
]


(* ::Subsection:: *)
(*One argument*)


TestMatch[
	DeclareObject["non-symbol"]
	,
	HoldPattern @ DeclareObject["non-symbol"]
	,
	Message[DeclareObject::sym, "non-symbol", 1]
	,
	TestID -> "1 arg: non-symbol: DeclareObject evaluation"
]

Module[
	{mockObj1, oldDefinition1}
	,
	declareMockObject[mockObj1];
	
	oldDefinition1 = ToString[Definition[mockObj1]];

	TestMatch[
		DeclareObject[mockObj1]
		,
		HoldPattern @ DeclareObject[mockObj1]
		,
		Message[Object::nonObject, 1, DeclareObject[mockObj1]]
		,
		TestID -> "1 arg: object: DeclareObject evaluation"
	];
	
	Test[
		ToString[Definition[mockObj1]]
		,
		oldDefinition1
		,
		TestID -> "1 arg: object: mockObj1 definition"
	];
]


(* ::Subsection:: *)
(*Two arguments*)


Module[
	{nonObject2, oldDefinition2}
	,
	oldDefinition2 = ToString[Definition[nonObject2]];

	TestMatch[
		DeclareObject["non-symbol", nonObject2]
		,
		HoldPattern @ DeclareObject["non-symbol", nonObject2]
		,
		{
			Message[DeclareObject::sym, "non-symbol", 1],
			Message[Object::object, 2, DeclareObject["non-symbol", nonObject2]]
		}
		,
		TestID -> "2 args: non-symbol, non-object: DeclareObject evaluation"
	];
	
	Test[
		ToString[Definition[nonObject2]]
		,
		oldDefinition2
		,
		TestID -> "2 args: non-symbol, non-object: nonObject2 definition"
	];
]

Module[
	{
		mockObj1, oldDefinition1,
		nonObject2, oldDefinition2
	}
	,
	declareMockObject[mockObj1];
	
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {mockObj1, nonObject2};
	
	TestMatch[
		DeclareObject[mockObj1, nonObject2]
		,
		HoldPattern @ DeclareObject[mockObj1, nonObject2]
		,
		{
			Message[Object::nonObject, 1, DeclareObject[mockObj1, nonObject2]],
			Message[Object::object, 2, DeclareObject[mockObj1, nonObject2]]
		}
		,
		TestID -> "2 args: object, non-object: DeclareObject evaluation"
	];
	
	Test[
		ToString @ Definition[mockObj1]
		,
		oldDefinition1
		,
		TestID -> "2 args: object, non-object: mockObj1 definition"
	];
	Test[
		ToString @ Definition[nonObject2]
		,
		oldDefinition2
		,
		TestID -> "2 args: object, non-object: nonObject2 definition"
	];
]


Module[
	{mockObj2, oldDefinition2}
	,
	declareMockObject[mockObj2];
	
	oldDefinition2 = ToString[Definition[mockObj2]];
	
	TestMatch[
		DeclareObject["non-symbol", mockObj2]
		,
		HoldPattern @ DeclareObject["non-symbol", mockObj2]
		,
		Message[DeclareObject::sym, "non-symbol", 1]
		,
		TestID -> "2 args: non-symbol, object: DeclareObject evaluation"
	];
	
	Test[
		ToString @ Definition[mockObj2]
		,
		oldDefinition2
		,
		TestID -> "2 args: non-symbol, object: mockObj2 definition"
	];
]

Module[
	{
		mockObj1, oldDefinition1,
		mockObj2, oldDefinition2
	}
	,
	Scan[declareMockObject, {mockObj1, mockObj2}];
	
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {mockObj1, mockObj2};
	
	TestMatch[
		DeclareObject[mockObj1, mockObj2]
		,
		HoldPattern @ DeclareObject[mockObj1, mockObj2]
		,
		Message[Object::nonObject, 1, DeclareObject[mockObj1, mockObj2]]
		,
		TestID -> "2 args: object, object: DeclareObject evaluation"
	];
	
	Test[
		ToString @ Definition[mockObj1]
		,
		oldDefinition1
		,
		TestID -> "2 args: object, object: mockObj1 definition"
	];
	Test[
		ToString @ Definition[mockObj2]
		,
		oldDefinition2
		,
		TestID -> "2 args: object, object: mockObj2 definition"
	];
]


Module[
	{
		nonObject1, oldDefinition1,
		nonObject2, oldDefinition2
	}
	,
	{oldDefinition1, oldDefinition2} =
		ToString[Definition[#]]& /@ {nonObject1, nonObject2};
	
	TestMatch[
		DeclareObject[nonObject1, nonObject2]
		,
		HoldPattern @ DeclareObject[nonObject1, nonObject2]
		,
		Message[Object::object, 2, DeclareObject[nonObject1, nonObject2]]
		,
		TestID ->
			"2 args: non-object symbol, non-object: DeclareObject evaluation"
	];
	
	Test[
		ToString @ Definition[nonObject1]
		,
		oldDefinition1
		,
		TestID ->
			"2 args: non-object symbol, non-object: nonObject1 definition"
	];
	Test[
		ToString @ Definition[nonObject2]
		,
		oldDefinition2
		,
		TestID ->
			"2 args: non-object symbol, non-object: nonObject2 definition"
	];
]


(* ::Subsection:: *)
(*Three arguments*)


Module[
	{
		sym1, oldDefinition1,
		mockObj2, oldDefinition2,
		mockObj3, oldDefinition3
	}
	,
	Scan[declareMockObject, {mockObj2, mockObj3}];
	
	{oldDefinition1, oldDefinition2, oldDefinition3} =
		ToString[Definition[#]]& /@ {sym1, mockObj2, mockObj3};
	
	TestMatch[
		DeclareObject[sym1, mockObj2, mockObj3]
		,
		HoldPattern @ DeclareObject[sym1, mockObj2, mockObj3]
		,
		Message[DeclareObject::argt, DeclareObject, 3, 1, 2]
		,
		TestID -> "3 args: DeclareObject evaluation"
	];
	
	Test[
		ToString @ Definition[sym1]
		,
		oldDefinition1
		,
		TestID -> "3 args: sym1 definition"
	];
	Test[
		ToString @ Definition[mockObj2]
		,
		oldDefinition2
		,
		TestID -> "3 args: mockObj2 definition"
	];
	Test[
		ToString @ Definition[mockObj3]
		,
		oldDefinition3
		,
		TestID -> "3 args: mockObj3 definition"
	];
]


(* ::Subsection:: *)
(*Correct arguments*)


(* ::Subsection:: *)
(*One argument*)


Module[
	{obj}
	,
	Test[
		DeclareObject[obj]
		,
		Null
		,
		TestID -> "1 arg: non-object symbol: DeclareObject evaluation"
	];
	
	TestMatch[
		UpValues[obj]
		,
		{
			HoldPattern[HoldPattern][HoldPattern[ObjectQ][obj]] :> True
			,
			HoldPattern[HoldPattern][HoldPattern[Super][obj]] :> Object
			,
			Sequence @@ setAlteringUpValues[obj]
		}
		,
		TestID -> "1 arg: non-object symbol: obj has proper up values"
	];
	TestMatch[
		DownValues[obj]
		,
		inheritanceDownValues[obj, Object]
		,
		TestID ->
			"1 arg: non-object symbol: obj has proper inheritance down values"
	];
]


(* ::Subsection:: *)
(*Two arguments*)


Module[
	{child, parent, parentOldDefinition}
	,
	declareMockObject[parent];
	parentOldDefinition = ToString[Definition[parent]];
	
	Test[
		DeclareObject[child, parent]
		,
		Null
		,
		TestID -> "2 args: non-object symbol, object: DeclareObject evaluation"
	];
	
	TestMatch[
		UpValues[child]
		,
		{
			HoldPattern[HoldPattern][HoldPattern[ObjectQ][child]] :> True
			,
			HoldPattern[HoldPattern][HoldPattern[Super][child]] :> parent
			,
			Sequence @@ setAlteringUpValues[child]
		}
		,
		TestID ->
			"2 args: non-object symbol, object: child has proper up values"
	];
	TestMatch[
		DownValues[child]
		,
		inheritanceDownValues[child, parent]
		,
		TestID -> "2 args: non-object symbol, object: \
child has proper inheritance down values"
	];
	
	Test[
		ToString @ Definition[parent]
		,
		parentOldDefinition
		,
		TestID -> "2 args: non-object symbol, object: parent definition"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
