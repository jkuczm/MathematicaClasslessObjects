(* ::Package:: *)

BeginPackage["ClasslessObjects`"]


(* ::Section:: *)
(*Usage messages*)


Unprotect[ObjectQ]
ClearAll[ObjectQ]

ObjectQ::usage =
"\
ObjectQ[x] \
returns True if x is an object and False otherwise."


Unprotect[Super]
ClearAll[Super]

Super::usage =
"\
Super[obj] \
returns parent of given object obj."


Unprotect[SetSuper]
ClearAll[SetSuper]

SetSuper::usage =
"\
SetSuper[obj, super] \
sets object super as parent of object obj."


Unprotect[DeclareObject]
ClearAll[DeclareObject]

DeclareObject::usage =
"\
DeclareObject[sym] \
declares symbol sym as object.\

DeclareObject[sym, super] \
declares symbol sym as object and sets super as it's parent."


Unprotect[WithOrdinaryObjectSet]
ClearAll[WithOrdinaryObjectSet]

WithOrdinaryObjectSet::usage =
"\
WithOrdinaryObjectSet[obj, body] \
evaluates body with switched off bindnig definitions of object obj. \
Returns result of body evaluation."


Unprotect[$self]
ClearAll[$self]

$self::usage =
"\
$self \
is a variable that, in the scope of bound member, is equal to object on \
which that member was called."


Unprotect[Object]
ClearAll[Object]

Object::usage =
"\
Object \
is an ancestor of all objects."


(* ::Section:: *)
(*Implementation*)
	

Begin["`Private`"]


(* ::Subsection:: *)
(*Private symbols usage*)


ClearAll[fixArgumentsNumber]

fixArgumentsNumber::usage =
"\
fixArgumentsNumber[sym, argNo] \
assign, to symbol sym, down value printing message informing about required \
number of arguments if number of arguments is different than argNo."


ClearAll[getInheritanceChain]

getInheritanceChain::usage =
"\
getInheritanceChain[obj] \
returns list of all ancestors of given object obj.\

getInheritanceChain[obj, ances] \
returns list of ancestors of given object obj up to object ances if ances is \
one of obj ancestors. Otherwise returns list of all ancestors."


ClearAll[withBoundSelf]

withBoundSelf::usage =
"\
withBoundSelf[self, body] \
evaluates body with $self variable representing given object self. \
Returns result of body evaluation."


ClearAll[setMember]

setMember::usage =
"\
setMember[obj, lhs, rhs] \
sets two down value definitions on obj ordinary definition: obj[lhs] and \
inheritable definition obj[lhs, self_] that allows objects inheriting from \
obj to call this member."


ClearAll[bindMember]

bindMember::usage =
"\
bindMember[obj, lhs, rhs] \
sets ordinary and inheritable down value definitions of lhs with right hand \
side rhs wrapped in Block providing $self variable."


ClearAll[unsetMember]

unsetMember::usage =
"\
unsetMember[obj, member] \
unsets ordinary: obj[member] and inheritable: obj[member, self_] definitions."


(* ::Subsection:: *)
(*fixArgumentsNumber*)


fixArgumentsNumber[sym_Symbol, argNo_Integer] :=
	With[
		{msgName = If[argNo === 1, sym::argx, sym::argrx]}
		,
		sym[args___ /; Length[{args}] != argNo] := "nothing" /;
			Message[msgName,
				HoldForm[sym],
				HoldForm[Evaluate @ Length[{args}]],
				HoldForm[argNo]
			];
	]


(* ::Subsection:: *)
(*ObjectQ*)


ObjectQ[_] = False

fixArgumentsNumber[ObjectQ, 1]


(* ::Subsection:: *)
(*Super*)


Super[_?ObjectQ] = Object

Super[arg_ /; !ObjectQ[arg]] := "nothing" /;
	Message[Object::object, HoldForm[1], HoldForm[Super[arg]]]

fixArgumentsNumber[Super, 1]


(* ::Subsection:: *)
(*getInheritanceChain*)


(*
	Using ObjectQ[ances] condition because MMA 8 chokes on
	ances:_?ObjectQ:Object
*)
getInheritanceChain[obj_?ObjectQ, ances_:Object] /; ObjectQ[ances] :=
	NestWhileList[Super, obj, !MatchQ[#, ances | Object] &]


(* ::Subsection:: *)
(*Object*)


Object::object = "Object expected at position `1` in `2`."

Object::nonObject = "Non-object expected at position `1` in `2`."

Object::objectMember =
"The call `1`@`2` didn't match any of defined member patterns in `1` nor in \
its ancestors: `3`."


SetAttributes[Object, HoldFirst]


Object[memberCall_, self_] :=
	With[
		{ancestors = Rest[getInheritanceChain[self]]}
		,
		Message[Object::objectMember,
			HoldForm[self], HoldForm[memberCall], HoldForm[ancestors]
		];
		
		$Failed
	]

fixArgumentsNumber[Object, 2]


ObjectQ[Object] ^= True


(* ::Subsection:: *)
(*WithOrdinaryObjectSet*)


SetAttributes[WithOrdinaryObjectSet, HoldRest]


WithOrdinaryObjectSet[obj_?ObjectQ, body_] :=
	Module[
		{
			upValues = UpValues[obj],
			protected,
			result
		}
		,
		protected = Unprotect[obj];
		(*
			All "set altering" definitions are attached to objects via
			UpValues, remove them temporarily.
		*)
		UpValues[obj] =
			FilterRules[
				UpValues[obj],
				Except[HoldPattern[HoldPattern][_Set | _SetDelayed | _Unset]]
			];
		Protect @@ protected;
		
		result = body;
		
		Unprotect @@ protected;
		UpValues[obj] = upValues;
		Protect @@ protected;
		
		result
	]
	
WithOrdinaryObjectSet[arg1_ /; !ObjectQ[arg1], arg2_] := "nothing" /;
	Message[Object::object,
		HoldForm[1], HoldForm[WithOrdinaryObjectSet[arg1, arg2]]
	]

fixArgumentsNumber[WithOrdinaryObjectSet, 2]


(* ::Subsection:: *)
(*SetSuper*)


SetSuper::cyclic =
"Object `1` can't inherit from object `2`, since it would lead to \
inheritance cycle `3`."


SetSuper[obj_?ObjectQ, super_?ObjectQ] := (
	With[
		{inheritanceChain = getInheritanceChain[super, obj]}
		,
		If[Last[inheritanceChain] === obj,
			Message[SetSuper::cyclic,
				HoldForm[obj], HoldForm[super], HoldForm[inheritanceChain]
			];
			Return[$Failed]
		]
	];
	
	If[super === Object,
		Quiet[
			obj /: Super[obj] =.
			,
			TagUnset::norep
		]
	(* else *),
		Super[obj] ^= super
	];
	
	WithOrdinaryObjectSet[obj,
		(* Delegate any undefinded member call to parent object. *)
		obj[x_, self_:obj] := super[x, self]
	]
)
	
SetSuper[arg1_ /; !ObjectQ[arg1], arg2_] := "nothing" /;
	Message[Object::object, HoldForm[1], HoldForm[SetSuper[arg1, arg2]]]

SetSuper[arg1_, arg2_ /; !ObjectQ[arg2]] := "nothing" /;
	Message[Object::object, HoldForm[2], HoldForm[SetSuper[arg1, arg2]]]

fixArgumentsNumber[SetSuper, 2]


(* ::Subsection:: *)
(*withBoundSelf*)


SetAttributes[withBoundSelf, HoldRest]


withBoundSelf[self_?ObjectQ, body_] :=
	Block[
		{$self = self}
		,
		$self /: Set[$self[lhs_], rhs_] := Set[self[lhs], rhs];
		$self /: SetDelayed[$self[lhs_], rhs_] := SetDelayed[self[lhs], rhs];
		$self /: Unset[$self[lhs_]] := Unset[self[lhs]];
		
		body
	]


(* ::Subsection:: *)
(*setMember*)


SetAttributes[setMember, HoldRest]


setMember[obj_?ObjectQ, lhs_, rhs_] :=
	WithOrdinaryObjectSet[obj,
		(* Don't duplicate Set::write warning if object is protected. *)
		Quiet[
			(* Definition that makes member inheritable. *)
			obj[lhs, _] = rhs
			,
			Set::write
		];
		
		(* Ordinary definition. *)
		obj[lhs] = rhs
	]


(* ::Subsection:: *)
(*bindMember*)


SetAttributes[bindMember, HoldRest]


bindMember[obj_?ObjectQ, lhs_, rhs_] :=
	WithOrdinaryObjectSet[obj,
		(* Don't duplicate SetDelayed::write warning if object is protected. *)
		Quiet[
			(* Inheritable definition providing $self variable. *)
			obj[lhs, self_] := withBoundSelf[self, rhs]
			,
			SetDelayed::write
		];
		
		(* Ordinary definition providing $self variable. *)
		obj[lhs] := withBoundSelf[obj, rhs]
	]


(* ::Subsection:: *)
(*unsetMember*)


SetAttributes[unsetMember, HoldRest]


unsetMember[obj_?ObjectQ, lhs_] :=
	WithOrdinaryObjectSet[obj,
		Module[
			{
				inherUnbound, inherBound, nonInher,
				norepCount = 0
			}
			,
			Quiet[
				(*
					Unset definition with unnamed second pattern.
					It exists for inheritable unbound members.
				*)
				Check[
					inherUnbound = (obj[lhs, _] =.),
					norepCount++,
					Unset::norep
				];
				(*
					Unset definitions with named second pattern.
					It exists for inheritable bound members.
				*)
				Check[
					inherBound = (obj[lhs, self_] =.),
					norepCount++,
					Unset::norep
				];
				,
				{Unset::norep, Unset::write}
			];
			
			(*
				Unset non-inheritable definition. If at leas one of prevoiusly
				unset definitions existed quiet Unset::norep message.
			*)
			nonInher =
				If[norepCount == 2,
					obj[lhs] =.
				(* else *),
					Quiet[obj[lhs] =., Unset::norep]
				];
			
			If[!MatchQ[Null, inherUnbound | inherBound | nonInher],
				(* All Upset evaluations failed so return $Failed. *)
				$Failed
			]
		]
	]


(* ::Subsection:: *)
(*DeclareObject*)


(*
	Using ObjectQ[super] condition because MMA 8 chokes on
	super:_?ObjectQ:Object
*)
DeclareObject[obj_Symbol /; !ObjectQ[obj], super:_:Object] /; ObjectQ[super] :=
	(
		ClearAll[obj];
		
		ObjectQ[obj] ^= True;
		SetSuper[obj, super];
		
		(* Make members created with Set automatically inheritable. *)
		obj /: Set[obj[lhs_], rhs_] := (setMember[obj, lhs, rhs]);
		
		(* Make members created with SetDelayed automatically bound. *)
		obj /: SetDelayed[obj[lhs_], rhs_] := (bindMember[obj, lhs, rhs]);
		
		(* Make sure unset removes ordinary and inheritable definitions. *)
		obj /: Unset[obj[lhs_]] := (unsetMember[obj, lhs]);
	)

DeclareObject[arg1:Except[_Symbol], Repeated[_, {0, 1}]] := "nothing" /;
	Message[DeclareObject::sym, HoldForm[arg1], HoldForm[1]]

DeclareObject[arg1_?ObjectQ, rest:Repeated[_, {0, 1}]] := "nothing" /;
	Message[Object::nonObject,
		HoldForm[1], HoldForm[DeclareObject[arg1, rest]]
	]

DeclareObject[arg1_, arg2_ /; !ObjectQ[arg2]] := "nothing" /;
	Message[Object::object, HoldForm[2], HoldForm[DeclareObject[arg1, arg2]]]

DeclareObject[args___ /; !MatchQ[Length[{args}], 1 | 2]] := "nothing" /;
	Message[DeclareObject::argt,
		HoldForm[DeclareObject],
		HoldForm[Evaluate @ Length[{args}]],
		HoldForm[1],
		HoldForm[2]
	]


End[]


(* ::Section:: *)
(*Public symbols protection*)


(*
	Protect all symbols in this context
	(all public symbols provided by this package)
*)
Protect["`*"]


EndPackage[]
