(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage[
	"ClasslessObjects`Tests`Integration`MembersExplicitSelf`",
	{"MUnit`"}
]


Get["ClasslessObjects`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Unrelated objects*)


DeclareObject[objA]
objA@changeAttr[x_] := ($self@attr = $self@attr + x)

DeclareObject[objB]
objB@changeAttr[x_] := ($self@attr = $self@attr - x)


(* ::Subsubsection:: *)
(*objA changeAttr called from objB*)


objA@attr = a
objB@attr = b

Test[
	objA[changeAttr[c], objB]
	,
	b + c
	,
	TestID -> "objA changeAttr called from objB"
]
Test[
	objA@attr
	,
	a
	,
	TestID -> "objA changeAttr called from objB: objA@attr"
]
Test[
	objB@attr
	,
	b + c
	,
	TestID -> "objA changeAttr called from objB: objB@attr"
]


(* ::Subsubsection:: *)
(*objB changeAttr called from objA*)


objA@attr = a
objB@attr = b

Test[
	objB[changeAttr[c], objA]
	,
	a - c
	,
	TestID -> "objB changeAttr called from objA"
]
Test[
	objA@attr
	,
	a - c
	,
	TestID -> "objB changeAttr called from objA: objA@attr"
]
Test[
	objB@attr
	,
	b
	,
	TestID -> "objB changeAttr called from objA: objB@attr"
]


(* ::Subsection:: *)
(*Inheriting objects*)


DeclareObject[grandParent]
grandParent@changeAttr[] := (
	$self@attr = Append[$self@attr, "grandParent called"];
)

DeclareObject[parent, grandParent]
parent@changeAttr[] := (
	grandParent[changeAttr[], $self];
	$self@attr = Append[$self@attr, "parent called"];
)

DeclareObject[child, parent]


(* ::Subsubsection:: *)
(*grandParent changeAttr called*)


grandParent@attr = {"grandParent"}
parent@attr = {"parent"}
child@attr = {"child"}

grandParent@changeAttr[]

Test[
	grandParent@attr
	,
	{"grandParent", "grandParent called"}
	,
	TestID ->
		"Inheriting objects: grandParent changeAttr called: grandParent@attr"
]
Test[
	parent@attr
	,
	{"parent"}
	,
	TestID -> "Inheriting objects: grandParent changeAttr called: parent@attr"
]
Test[
	child@attr
	,
	{"child"}
	,
	TestID -> "Inheriting objects: grandParent changeAttr called: child@attr"
]


(* ::Subsubsection:: *)
(*parent changeAttr called*)


grandParent@attr = {"grandParent"}
parent@attr = {"parent"}
child@attr = {"child"}

parent@changeAttr[]

Test[
	grandParent@attr
	,
	{"grandParent"}
	,
	TestID -> "Inheriting objects: parent changeAttr called: grandParent@attr"
]
Test[
	parent@attr
	,
	{"parent", "grandParent called", "parent called"}
	,
	TestID -> "Inheriting objects: parent changeAttr called: parent@attr"
]
Test[
	child@attr
	,
	{"child"}
	,
	TestID -> "Inheriting objects: parent changeAttr called: child@attr"
]


(* ::Subsubsection:: *)
(*child changeAttr called*)


grandParent@attr = {"grandParent"}
parent@attr = {"parent"}
child@attr = {"child"}

child@changeAttr[]

Test[
	grandParent@attr
	,
	{"grandParent"}
	,
	TestID -> "Inheriting objects: child changeAttr called: grandParent@attr"
]
Test[
	parent@attr
	,
	{"parent"}
	,
	TestID -> "Inheriting objects: child changeAttr called: parent@attr"
]
Test[
	child@attr
	,
	{"child", "grandParent called", "parent called"}
	,
	TestID -> "Inheriting objects: child changeAttr called: child@attr"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*"]
Quiet[Remove["`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
