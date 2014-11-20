(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`SettersOneLevelInheritance`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


setUpObjects[setterObj_, setterChildObj_, otherObj_] := (
	DeclareObject[setterObj];
	DeclareObject[setterChildObj, setterObj];
	DeclareObject[otherObj];
)

setUpObjects[setterObj_, setterChildObj_, explSelfObj_, otherObj_] := (
	setUpObjects[setterObj, setterChildObj, otherObj];
	DeclareObject[explSelfObj];
)


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Unbound inheritable*)


(* ::Subsubsection:: *)
(*setterObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] := ($self@member = {$self, Super[$self], x});
	
	Test[
		setterObj@setter[arg]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call setter"
	];
	
	Test[
		setterObj@member
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] := ($self@member = {$self, Super[$self], x});
	
	Test[
		setterChildObj@setter[arg]
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj explicit self*)


Module[
	{setterObj, setterChildObj, explSelfObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, explSelfObj, otherObj];
	setterObj@setter[x_] := ($self@member = {$self, Super[$self], x});
	
	Test[
		setterChildObj[setter[arg], explSelfObj]
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self explSelfObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self setterObj"
	];
	
	Test[
		explSelfObj@member
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member, otherObj]
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member, setterObj]
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterObj"
	];
	Test[
		explSelfObj[member, setterChildObj]
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Unbound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterChildObj"
	];
]


(* ::Subsection:: *)
(*Bound inheritable*)


(* ::Subsubsection:: *)
(*setterObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg, arg2}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] := ($self@member[y_] := {$self, Super[$self], x, y});
	
	Test[
		setterObj@setter[arg]
		,
		Null
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		{setterObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		{otherObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		{setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		{setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		{otherObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		{setterObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg, arg2}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] := ($self@member[y_] := {$self, Super[$self], x, y});
	
	Test[
		setterChildObj@setter[arg]
		,
		Null
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		{setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		{otherObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		{setterObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj explicit self*)


Module[
	{
		setterObj, setterChildObj, explSelfObj, otherObj,
		member, setter, arg, arg2
	}
	,
	setUpObjects[setterObj, setterChildObj, explSelfObj, otherObj];
	setterObj@setter[x_] := ($self@member[y_] := {$self, Super[$self], x, y});
	
	Test[
		setterChildObj[setter[arg], explSelfObj]
		,
		Null
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self explSelfObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self setterObj"
	];
	
	Test[
		explSelfObj@member[arg2]
		,
		{explSelfObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member[arg2], otherObj]
		,
		{otherObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member[arg2], setterObj]
		,
		{setterObj, Object, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterObj"
	];
	Test[
		explSelfObj[member[arg2], setterChildObj]
		,
		{setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Bound inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterObj"
	];
]


(* ::Subsection:: *)
(*Non-inheritable*)


(* ::Subsubsection:: *)
(*setterObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self@member = {$self, Super[$self], x}
		];
	
	Test[
		setterObj@setter[arg]
		,
		{setterObj, Object, arg}
		,
		TestID -> "Non-inheritable: setterObj implicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		{setterObj, Object, arg}
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self@member = {$self, Super[$self], x}
		];
	
	Test[
		setterChildObj@setter[arg]
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		{setterChildObj, setterObj, arg}
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj explicit self*)


Module[
	{setterObj, setterChildObj, explSelfObj, otherObj, member, setter, arg}
	,
	setUpObjects[setterObj, setterChildObj, explSelfObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self@member = {$self, Super[$self], x}
		];
	
	Test[
		setterChildObj[setter[arg], explSelfObj]
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: call setter"
	];
	
	Test[
		setterObj@member
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	Test[
		setterObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member, explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self explSelfObj"
	];
	Test[
		setterChildObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member setterChildObj: explicit self setterObj"
	];
	
	Test[
		explSelfObj@member
		,
		{explSelfObj, Object, arg}
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member, otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member, setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member, {Object}]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterObj"
	];
	Test[
		explSelfObj[member, setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member, {setterObj, Object}
		]
		,
		TestID -> "Non-inheritable: setterChildObj explicit self: \
call member explSelfObj: explicit self setterChildObj"
	];
]


(* ::Subsection:: *)
(*Inheritable-only*)


(* ::Subsubsection:: *)
(*setterObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg, arg2}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self[member[y_], self_] := {$self, self, Super[self], x, y}
		];
	
	Test[
		setterObj@setter[arg]
		,
		Null
		,
		TestID -> "Inheritable-only: setterObj implicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		{$self, otherObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		{$self, setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		{$self, setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		{$self, otherObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		{$self, setterObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj implicit self*)


Module[
	{setterObj, setterChildObj, otherObj, member, setter, arg, arg2}
	,
	setUpObjects[setterObj, setterChildObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self[member[y_], self_] := {$self, self, Super[self], x, y}
		];
	
	Test[
		setterChildObj@setter[arg]
		,
		Null
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		{$self, otherObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		{$self, setterObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterChildObj implicit self: \
call member setterChildObj: explicit self setterObj"
	];
]


(* ::Subsubsection:: *)
(*setterChildObj explicit self*)


Module[
	{
		setterObj, setterChildObj, explSelfObj, otherObj,
		member, setter, arg, arg2
	}
	,
	setUpObjects[setterObj, setterChildObj, explSelfObj, otherObj];
	setterObj@setter[x_] :=
		WithOrdinaryObjectSet[$self,
			$self[member[y_], self_] := {$self, self, Super[self], x, y}
		];
	
	Test[
		setterChildObj[setter[arg], explSelfObj]
		,
		Null
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: call setter"
	];
	
	Test[
		setterObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterObj"
	];
	Test[
		setterObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterObj: explicit self otherObj"
	];
	Test[
		setterObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterObj: explicit self explSelfObj"
	];
	Test[
		setterObj[member[arg2], setterChildObj]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterObj: explicit self setterChildObj"
	];
	
	Test[
		setterChildObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember,
			setterChildObj, member[arg2], {setterObj, Object}
		]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterChildObj"
	];
	Test[
		setterChildObj[member[arg2], otherObj]
		,
		$Failed
		,
		Message[Object::objectMember, otherObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterChildObj: explicit self otherObj"
	];
	Test[
		setterChildObj[member[arg2], explSelfObj]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterChildObj: explicit self explSelfObj"
	];
	Test[
		setterChildObj[member[arg2], setterObj]
		,
		$Failed
		,
		Message[Object::objectMember, setterObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member setterChildObj: explicit self setterObj"
	];
	
	Test[
		explSelfObj@member[arg2]
		,
		$Failed
		,
		Message[Object::objectMember, explSelfObj, member[arg2], {Object}]
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member explSelfObj"
	];
	Test[
		explSelfObj[member[arg2], otherObj]
		,
		{$self, otherObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member explSelfObj: explicit self otherObj"
	];
	Test[
		explSelfObj[member[arg2], setterObj]
		,
		{$self, setterObj, Object, arg, arg2}
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member explSelfObj: explicit self setterObj"
	];
	Test[
		explSelfObj[member[arg2], setterChildObj]
		,
		{$self, setterChildObj, setterObj, arg, arg2}
		,
		TestID -> "Inheritable-only: setterChildObj explicit self: \
call member explSelfObj: explicit self setterChildObj"
	];
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
