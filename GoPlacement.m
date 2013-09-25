(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
BeginPackage["GoPlacement`", {"CoreFurniturePlacement`"}]
(*
calculate::usage =
        "calculate[room,doors,furnitures] 
calculate the graphics elements according to the room&furnitures sizes."
*)
gomain::usage =
        "Lanch the application place furniture."


Begin["`Private`"]


gomain[]:=
DynamicModule[{eleName,eleLength,eleWidth,eleHeight,eleId=0,eleNear,roomLength,roomWidth,roomHeight,doorLength,doorHeight,doorXCoordinate,doorYCoordinate,doorId=100,doorName,eleDel,gra,roomlen=10,roomwid=10,roomhei=10},
elementsDisList={};
doorDisList={};
elementsList={};
roomList={};
doorList={};
gra={Text[Style["ROOM",FontSize->20],{roomlen/2,roomwid/2,roomhei/2}]};
fGetPart[list_,index_]:=Table[Part[list,i,index],{i,Length[list]}];
fGetPartRoom[list_,index_]:=If[Length[list]>0,Part[list,index]];
fCreatDialoge[type_]:=Which[
type==1,CreateDialog[{TextCell["Please input all attributes"]}],
type==2,CreateDialog[{TextCell["Characters need less than 15"]}],
type==3,CreateDialog[{TextCell["You can add most 10 furnitures"]}],
type==4,CreateDialog[{TextCell["You can add most 2 doors"]}],
type==5,CreateDialog[{TextCell["You need set room,door,furniture"]}],
type==6,CreateDialog[{TextCell["The door is bigger than the room"]}],
type==7,CreateDialog[{TextCell["The furniture is bigger than the room"]}]
];
Deploy[Style[Panel[Row[{Panel[Column[{Grid[
Join[
Append[Prepend[Transpose[{{"length","width","height"},{
InputField[Dynamic[roomLength],Number,FieldHint->"enter length"],
InputField[Dynamic[roomWidth],Number,FieldHint->"enter width"],
InputField[Dynamic[roomHeight],Number,FieldHint->"enter height"]}}],
{Style["Set Room",Bold]}],
{Button["Save",If[NumberQ[roomLength]&&NumberQ[roomWidth]&&NumberQ[roomHeight]&&roomLength>0&&roomWidth>0&&roomHeight>0,roomList={roomLength,roomWidth,roomHeight},fCreatDialoge[1]]],
Button["Clear",{Clear[roomLength,roomWidth,roomHeight]}]}],
Append[
Prepend[
Transpose[{
{"length","height","X-coordinate","Y-coordinate","name"},
{
InputField[Dynamic[doorLength],Number,FieldHint->"enter length"],
InputField[Dynamic[doorHeight],Number,FieldHint->"enter height"],
InputField[Dynamic[doorXCoordinate],Number,FieldHint->"enter x-coordinate"],
InputField[Dynamic[doorYCoordinate],Number,FieldHint->"enter y-coordinate"],
InputField[Dynamic[doorName],String,FieldHint->"enter name"]
}
}]
,{Style["Add Doors",Bold]}]
,{
Button["Save",
If[NumberQ[doorLength]&&NumberQ[doorHeight]&&NumberQ[doorXCoordinate]&&NumberQ[doorYCoordinate]&&doorLength>0&&doorHeight>0&&StringQ[doorName]&&StringLength[doorName]>0,
If[Length[roomList]==0||Length[roomList]>0&&doorLength<roomLength&&doorHeight<roomHeight,
If[StringLength[doorName]<=15,
If[Length[doorDisList]<2,doorId++;doorDisList=Append[doorDisList,{doorXCoordinate,doorYCoordinate,doorLength,doorHeight,doorName,doorId}],fCreatDialoge[4]]
,fCreatDialoge[2]]
,fCreatDialoge[6]]
,fCreatDialoge[1]]
],
Button["Clear",{Clear[doorLength,doorHeight,doorXCoordinate,doorYCoordinate,doorName]}]
}
],Append[Prepend[Transpose[{{"length","width","height","name","neartoID"},{
InputField[Dynamic[eleLength],Number,FieldHint->"enter length"],
InputField[Dynamic[eleWidth],Number,FieldHint->"enter width"],
InputField[Dynamic[eleHeight],Number,FieldHint->"enter height"],
InputField[Dynamic[eleName],String,FieldHint->"enter name"],
Dynamic[PopupMenu[Dynamic[eleNear],Join[Flatten[{Null,Table[Part[doorDisList,i,6],{i,Length[doorDisList]}]}],Table[Part[elementsDisList,i,6],{i,Length[elementsDisList]}]]
]]}}],{Style["Add Furnitures",Bold]}],
{Button["Save",
If[NumberQ[eleLength]&&NumberQ[eleWidth]&&NumberQ[eleHeight]&&eleLength>0&&eleWidth>0&&eleHeight>0&&StringQ[eleName]&&StringLength[eleName]>0,
If[Length[roomList]==0||Length[roomList]>0&&eleLength<roomLength&&eleWidth<roomWidth&&eleHeight<roomHeight,
If[StringLength[eleName]<=15,
If[Length[elementsDisList]<10,eleId++;
If[!NumberQ[eleNear],
elementsDisList=Append[elementsDisList,{eleLength,eleWidth,eleHeight,eleName,"NONE",eleId}],
elementsDisList=Append[
elementsDisList,
{eleLength,eleWidth,eleHeight,eleName,eleNear,eleId}
]
]
,fCreatDialoge[3]]
,fCreatDialoge[2]]
,fCreatDialoge[7]]
,fCreatDialoge[1]];eleNear=NULL]
,Button["Clear",{Clear[eleLength,eleWidth,eleHeight,eleName];eleNear=Null}]}],
Append[
Prepend[
Transpose[{{"Select"},
{Dynamic[PopupMenu[Dynamic[eleDel],Join[Flatten[{Null,Table[Part[doorDisList,i,6],{i,Length[doorDisList]}]}],Flatten[{Table[Part[elementsDisList,i,6],{i,Length[elementsDisList]}]}]]
]]}}],{Style["Delete Furniture",Bold]}],
{Button["Confirm",
For[i=1,i<=Length[elementsDisList],i++,If[elementsDisList[[i,6]]==eleDel,elementsDisList=Drop[elementsDisList,{i}]]];
For[i=1,i<=Length[elementsDisList],i++,
If[elementsDisList[[i,6]]>eleDel,elementsDisList=ReplacePart[elementsDisList,{i,6}->elementsDisList[[i,6]]-1]];
If[elementsDisList[[i,5]]==eleDel,elementsDisList[[i,5]]="NONE"];
If[elementsDisList[[i,5]]>eleDel&&elementsDisList[[i,5]]<20,elementsDisList=ReplacePart[elementsDisList,{i,5}->elementsDisList[[i,5]]-1]]
];
(*If[elementsDisList[[i,5]]>eleDel&&eleDel>20,elementsDisList=ReplacePart[elementsDisList,{i,5}->elementsDisList[[i,5]]-1]];*)
If[Length[elementsDisList]>0,eleId=elementsDisList[[Length[elementsDisList],6]],eleId=0];
For[i=1,i<=Length[doorDisList],i++,
If[
Part[doorDisList,i,6]==eleDel,doorDisList=Drop[doorDisList,{i}];
Which[
Length[doorDisList]==0,doorId=100,
eleDel==101,doorId=100,
eleDel==102,doorId=101
]
(*If[eleDel==21,doorId=20,doorId=21]*)
]];
finalList={}
]}],
{{Button["Submit",
If[Length[roomList]>0&&Length[doorDisList]>0&&Length[elementsDisList]>0,For[i=1,i<=Length[elementsDisList],i++,
If[
Part[elementsDisList,i,-2]!="NONE",
elementsList=Append[elementsList,ReplacePart[Drop[Part[elementsDisList,i],-1],-1->"NEARTO_"<>ToString[elementsDisList[[i,-2]]]]]
,elementsList=Append[elementsList,Drop[Part[elementsDisList,i],-1]]
]
];
For[i=1,i<=Length[doorDisList],i++,doorList=Append[doorList,Drop[Part[doorDisList,i],-1]]];finalList={roomList,doorList,elementsList};elementsList={};doorList={};


(**Print[finalList];**)
gra=calculate[finalList[[1]],finalList[[2]],finalList[[3]] ];
roomlen=finalList[[1]][[1]] ;
roomwid=finalList[[1]][[2]] ;
roomhei=finalList[[1]][[3]] ;
(**Print[gra];**)


,fCreatDialoge[5]
]
]}}],Dividers->{{False},{6->True,13->True,20->True,23->True}},Alignment->{{Left,Left}}(*,Spacings->{Automatic,0.1}*)]}],ImageSize->{310,640}],Column[{Dynamic[Panel[Grid[Join[{{Style["Room",Bold]}},{{"length","width","height"}},{{fGetPartRoom[roomList,1],fGetPartRoom[roomList,2],fGetPartRoom[roomList,3]}}]],ImageSize->{350,80}]],Dynamic[Panel[Grid[Join[{{Style["Doors",Bold]}},{{"ID","name","length","height","X","Y"}},Transpose[{fGetPart[doorDisList,6],fGetPart[doorDisList,5],fGetPart[doorDisList,3],fGetPart[doorDisList,4],fGetPart[doorDisList,1],fGetPart[doorDisList,2]}]]],ImageSize->{350,110}]],Dynamic[Panel[Grid[Join[{{Style["Elements",Bold]}},{{"ID","name","length","width","height","nearto"}},Transpose[{fGetPart[elementsDisList,6],fGetPart[elementsDisList,4],fGetPart[elementsDisList,1],fGetPart[elementsDisList,2],fGetPart[elementsDisList,3],fGetPart[elementsDisList,5]}]]],ImageSize->{350,440}]]},Alignment->Top]}]],(*\:4fee\:6539\:8868\:683c\:5355\:5143*)DefaultOptions->{InputField->{FieldSize->14},Button->{ImageSize->80}}]]

Dynamic[
Column[
{
Graphics3D[
gra,
ImageSize->400,
PlotRange-> {{0,roomlen},{0,roomwid},{0,roomhei}},
PlotLabel-> "Roomstyle"
]
}
]
]

]


End[ ]

EndPackage[ ]

