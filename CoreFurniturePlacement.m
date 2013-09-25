(* ::Package:: *)

BeginPackage["CoreFurniturePlacement`"]

calculate::usage =
        "calculate[room,doors,furnitures] 
calculate the graphics elements according to the room&furnitures sizes."



Begin["`Private`"]





calculate[room_,doors_,furnitures_]:=
Module[{gra1,gra2,direction},

roomMap= ConstantArray[0,{room[[2]],room[[1]]}];
FLAGANTAREA = 99;(*not bigger than 100*)

MapIndexed[mAllocDoors,doors];
MapIndexed[mAllocRandom,furnitures];
gra1=MapIndexed[GetObjByid,furnitures];
gra2=MapIndexed[GetDoorsByid,doors] ;
direction ={{ Text["Nord",{0,room[[2]]/2+50,room[[3]]-10}],Arrow[{{0,room[[2]]/2-30,room[[3]]-10},{0,room[[2]]/2+30,room[[3]]-10}}]
}};
Return[Join[gra1,gra2,direction]]
]

getCenterFromRange = Function[{y1,y2,x1,x2},
Module[{cx,cy},
cx=Round[(Abs[x1-x2])/2+x1];
cy=Round[(Abs[y1-y2])/2+y1];
{cx,cy}
]
];
getCenterFromRange = Function[{y1,y2,x1,x2},
Module[{cx,cy},
cx=Round[(Abs[x1-x2])/2+x1];
cy=Round[(Abs[y1-y2])/2+y1];
{cx,cy}
]
];
getDistance=Function[{x1,y1,x2,y2},
Module[{res},
res = Sqrt[(x1-x2)^2+(y1-y2)^2]
]
];
getRefid=Function[{str},
Module[{len, pos, id},
id=0;
len = StringLength[str];
pos = StringPosition[str, "NEARTO_"];
If[pos=={{1,7}},
id = StringTake[str, {pos[[1]][[2]] + 1, len}];
];
{id}
]
];
mapIterateNorth[a_,b_,id_,roomlen_,considerAntArea_]:=(
Module[{res,subarea,antarea,c,roomwid},

(*Print["a,b,id",{a,b,id},"mapping in n"];*)
roomwid=Length[roomMap];
res=False;
c=Round[a/2];

If[b+c>= roomwid,Print["furniture with id",id," is too big in the room in north direction ... "];,

If[considerAntArea=="considerAntarea",
For[i=1,i+a-1<= roomlen,i++,
subarea=roomMap[[1;;b,i;;(i+a-1)]];
antarea=roomMap[[b+1;;b+c,i;;(i+a-1)]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
roomMap[[1;;b,i;;(i+a-1)]]=id;
roomMap[[b+1;;b+c,i;;(i+a-1)]]= FLAGANTAREA;
res=True;
Break[];
];
];,
For[i=1,i+a-1<= roomlen,i++,
subarea=roomMap[[1;;b,i;;(i+a-1)]];
If[FreeQ[Flatten[subarea],n_/;n>0],
roomMap[[1;;b,i;;(i+a-1)]]=id;
res=True;
Break[];
];
];
];


];


res
]
)
mapIterateWest[a_,b_,id_,roomwid_,considerAntArea_]:=(

(*Print["a,b,id",{a,b,id},"mapping in w"];*)

Module[{res,subarea,antarea,c,roomlen},
res=False;
c=Round[a/2];
roomlen =Length[roomMap[[1]]];

If[b+c>= roomlen,Print["furniture with id",id," is too big in the room in west direction ... "];,

If[considerAntArea=="considerAntarea",
For[i=1,i+a-1<= roomwid,i++,
subarea=roomMap[[i;;(i+a-1),1;;b]];
antarea=roomMap[[i;;(i+a-1),b+1;;b+c]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
roomMap[[i;;(i+a-1),1;;b]]=id;
roomMap[[i;;(i+a-1),b+1;;b+c]]=FLAGANTAREA;
res=True;
Break[];
];
];,
For[i=1,i+a-1<= roomwid,i++,
subarea=roomMap[[i;;(i+a-1),1;;b]];
If[FreeQ[Flatten[subarea],n_/;n>0],
roomMap[[i;;(i+a-1),1;;b]]=id;
res=True;
Break[];
];
];
];
];
res
]
)
mapIterateEast[a_,b_,id_,roomlen_,roomwid_,considerAntArea_]:=(

(*Print["a,b,id",{a,b,id},"mapping in e"];*)

Module[{res,subarea,antarea,c},
res=False;
c=Round[a/2];

If[b+c>= roomwid,Print["furniture with id",id," is too big in the room in east direction... "];,

If[considerAntArea=="considerAntarea",
For[i=1,i+a-1<= roomwid,i++,
subarea=roomMap[[i;;(i+a-1),roomlen-b+1;;roomlen]];
antarea=roomMap[[i;;(i+a-1),roomlen-b+1-c;;roomlen-b]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
roomMap[[i;;(i+a-1),roomlen-b+1;;roomlen]]=id;
roomMap[[i;;(i+a-1),roomlen-b+1-c;;roomlen-b]]=FLAGANTAREA;
res=True;
Break[];
];
];,
For[i=1,i+a-1<= roomwid,i++,
subarea=roomMap[[i;;(i+a-1),roomlen-b+1;;roomlen]];
If[FreeQ[Flatten[subarea],n_/;n>0],
roomMap[[i;;(i+a-1),roomlen-b+1;;roomlen]]=id;
res=True;
Break[];
];
];
];
];
res
]
)
mapIterateSouth[a_,b_,id_,roomlen_,roomwid_,considerAntArea_]:=(

(*Print["a,b,id",{a,b,id},"mapping in s"];*)

Module[{res,subarea,antarea,c},
res=False;
c=Round[a/2];


If[b+c>= roomlen,Print["furniture with id",id," is too big in the room in south direction ... "];,


If[considerAntArea=="considerAntarea",
For[i=1,i+a-1<= roomlen,i++,
subarea=roomMap[[roomwid-b+1;;roomwid,i;;i+a-1]];
antarea=roomMap[[roomwid-b+1-c;;roomwid-b,i;;i+a-1]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
roomMap[[roomwid-b+1;;roomwid,i;;i+a-1]]=id;
roomMap[[roomwid-b+1-c;;roomwid-b,i;;i+a-1]]=FLAGANTAREA;
res=True;
Break[];
];
];,
For[i=1,i+a-1<= roomlen,i++,
subarea=roomMap[[roomwid-b+1;;roomwid,i;;i+a-1]];
If[FreeQ[Flatten[subarea],n_/;n>0],
roomMap[[roomwid-b+1;;roomwid,i;;i+a-1]]=id;
res=True;
Break[];
];
];
];
];
res
]
)
mapBedWithFengshui[a_,b_,id_,dir_]:=(
Module[{roomlen,roomwid},
roomlen =Length[roomMap[[1]]];
roomwid=Length[roomMap];
Which[
dir =="n",
mapIterateNorth[b,a,id,roomlen,"notconsiderAntarea"],
dir=="e",
mapIterateEast[a,b,id,roomlen,roomwid,"notconsiderAntarea"],
dir=="s",
mapIterateSouth[b,a,id,roomlen,roomwid,"notconsiderAntarea"],
dir=="w",
mapIterateWest[a,b,id,roomwid,"notconsiderAntarea"]
]
]
)

mapObj=Function[{len,wid,id,dir},
Module[{roomlen,roomwid,faces,a,b,res},
roomlen =Length[roomMap[[1]]];
roomwid=Length[roomMap];
a=len;
b=wid;
res=False;
If[dir=="n",
res= mapIterateNorth[a,b,id,roomlen,"considerAntarea"]; 
If[MemberQ[res,False],res=mapIterateNorth[b,a,id,roomlen,"considerAntarea"];]
];
If[dir=="w",
res= mapIterateWest[a,b,id,roomwid,"considerAntarea"]; 
If[MemberQ[res,False],res=mapIterateWest[b,a,id,roomwid,"considerAntarea"];]
];
If[dir=="s",
res= mapIterateSouth[a,b,id,roomlen,roomwid,"considerAntarea"]; 
If[MemberQ[res,False],res=mapIterateSouth[b,a,id,roomlen,roomwid,"considerAntarea"];]
];
If[dir=="e",
res= mapIterateEast[a,b,id,roomlen,roomwid,"considerAntarea"]; 
If[MemberQ[res,False],res=mapIterateEast[b,a,id,roomlen,roomwid,"considerAntarea"];]
];
res
]
];

mAllocRandom= Function[
{descriptions,index},
Module[{a,b,id,refid,dirs,dir,name,res},
a=descriptions[[1]];
b=descriptions[[2]];
id=index[[1]];
name=descriptions[[4]];
dirs={"e","s","w","n"};
refid=getRefid[Last[descriptions]];

If[refid[[1]]!= 0,(*map obj near to the rif obj*)
mapnear[id,descriptions,refid];,
While[Length[dirs]!= 0,(*map obj in random direction*)
dir = RandomSample[dirs,1][[1]];
dirs=DeleteCases[dirs,dir];
If[
StringMatchQ[name,"bedFS"~~___],
res=mapBedWithFengshui[a,b,id,dir];,
res=mapObj[a,b,id,dir];
];
If[res==True,Break[];];
];
];
]
];

mAllocDoors=Function[{descriptions,index},
Module[{roomlen,roomwid,x,y,len,id},
roomlen =Length[roomMap[[1]]];
roomwid=Length[roomMap];
x=descriptions[[1]];
y=descriptions[[2]];
len=descriptions[[3]];
id=100+index[[1]];
Which[
x==0,
If[roomwid-y-len+1<= 1||len+1>= roomlen,Print["door size is not real"];,
roomMap[[roomwid-y-len+1;;roomwid-y,x+1;;x+1]]=id;
roomMap[[roomwid-y-len+1;;roomwid-y,x+2;;len+1]]=FLAGANTAREA;
];

,x== roomlen,
If[roomwid-y-len+1<= 1,Print["door size is not real"];,
roomMap[[roomwid-y-len+1;;roomwid-y,x;;x]]=id;
roomMap[[roomwid-y-len+1;;roomwid-y,x-len;;x-1]]=FLAGANTAREA;
];
,y==0,
If[roomwid-y-len<= 0||x+len>= roomlen,Print["door size is not real"];,
roomMap[[roomwid-y;;roomwid-y,x+1;;x+len]]=id;
roomMap[[roomwid-y-len;;roomwid-y-1,x+1;;x+len]]=FLAGANTAREA;
];
,y==roomwid,
If[roomwid-y+len>= roomwid||x+len>= roomlen,Print["door size is not real"];,
roomMap[[roomwid-y+1;;roomwid-y+1,x+1;;x+len]]=id;
roomMap[[roomwid-y+2;;roomwid-y+len,x+1;;x+len]]=FLAGANTAREA
];
];
];
];

GetObjByid=Function[{desc,ids},
Module[{id,area,keyx,xline,keyy,xlen,ylen,x,y,z,colors,color,name},
id=ids[[1]];
area=Position[roomMap,id];
If[area=={},Print["obj non mapped ... id",id];,
keyx=Last[area][[1]];
xline=Select[area,#[[1]]==keyx&];
keyy=First[xline][[2]];
xlen = Length[xline];
ylen=Length[area]/xlen;
y=Length[roomMap]- keyx;
x=keyy-1;
z=desc[[3]];
name=desc[[4]];
{Cuboid[{x,y,0},{x+xlen,y+ylen,z}],
Text[name,{x+xlen/2,y+ylen/2,z/2}]}
]
]
];

GetDoorsByid=Function[{desc,ids},
Module[{id,area,keyx,xline,keyy,xlen,ylen,x,y,z,name,dir},
id=100+ids[[1]];
area=Position[roomMap,id];

If[area=={},Print["door with id",id," has not been mapped."];,

keyx=Last[area][[1]];
xline=Select[area,#[[1]]==keyx&];
keyy=First[xline][[2]];
xlen = Length[xline];
ylen=Length[area]/xlen;
y=Length[roomMap]- keyx;
x=keyy-1;
z=desc[[4]];
name=desc[[5]];

Which[
(y==0||y==Length[roomMap]-1),
{ Opacity[0.5],
Polygon[{{x,y,0},{x, y,z},{x+xlen, y,z},{x+xlen,y,0}}],
Text[name,{x+xlen/2,y+ylen/2,z/2}]
},
(x==0||x==Length[roomMap[[1]]]-1),
{ Opacity[0.5],
Polygon[{{x,y,0},{x, y,z},{x, y+ylen,z},{x,y+ylen,0}}],
Text[name,{x+xlen/2,y+ylen/2,z/2}]
}
]
]

]
];

mapnear=Function[{objid,descr,ref},
Module[{roomlen,roomwid,refid,refArea,lty,ltx,lbx,lby,rbx,rby,
rbkey,bline,cx,cy,a,b,c,x1,x2,y1,y2,x11,x22,y11,y22,subarea,antarea,minDistance,tmpcenter,tmpdistance,tmplastdis,storage},
roomlen =Length[roomMap[[1]]];
roomwid=Length[roomMap];
refid=ToExpression[ref[[1]]];
refArea=Position[roomMap,refid];

If[refArea=={},Print["obj not present in area, obj id:",ref];,
lty=refArea[[1]][[1]];
ltx=refArea[[1]][[2]];
rbkey=Last[refArea][[1]];

bline=Select[refArea,#[[1]]==rbkey&];
lby=bline[[1]][[1]];
lbx=bline[[1]][[2]];
rby=rbkey;
rbx=Last[refArea][[2]];
cx=Round[(Abs[lbx-rbx])/2+ltx];
cy=Round[(Abs[lty-lby])/2+lty];

a=descr[[1]];
b=descr[[2]];
c=Round[a/2];
minDistance=roomlen;

(*north iteration*)
tmplastdis=0;
For[i=1,i+a-1<= roomlen,i++,
y1=1;
y2=b;
x1=i;
x2=(i+a-1);
x11=i;
x22=(i+a-1);
y11=b+1;
y22=b+c;
subarea=roomMap[[y1;;y2,x1;;x2]];
antarea=roomMap[[y11;;y22,x11;;x22]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
tmpcenter = getCenterFromRange[y1,y2,x1,x2];
tmpdistance=getDistance[cx,cy,tmpcenter [[1]],tmpcenter [[2]] ];

If[tmplastdis==0,tmplastdis=tmpdistance;,
If[tmplastdis<tmpdistance,Break[];,tmplastdis=tmpdistance;];
];

If[tmpdistance<minDistance,
minDistance=tmpdistance;
storage={{y1,y2,x1,x2},{y11,y22,x11,x22}};
];
]
];

(* west iteration*)
tmplastdis=0;
For[i=1,i+a-1<= roomwid,i++,
y1=i;
y2=(i+a-1);
x1=1;
x2=b;
y11=i;
y22=(i+a-1);
x11=b+1;
x22=b+c;
subarea=roomMap[[y1;;y2,x1;;x2]];
antarea=roomMap[[y11;;y22,x11;;x22]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
tmpcenter = getCenterFromRange[y1,y2,x1,x2];
tmpdistance=getDistance[cx,cy,tmpcenter [[1]],tmpcenter [[2]] ];

If[tmplastdis==0,tmplastdis=tmpdistance;,
If[tmplastdis<tmpdistance,Break[];,tmplastdis=tmpdistance;];
];

If[tmpdistance<minDistance,
minDistance=tmpdistance;
storage={{y1,y2,x1,x2},{y11,y22,x11,x22}};
];
]
];

(* east iteration*)
tmplastdis=0;
For[i=1,i+a-1<= roomwid,i++,
y1=i;
y2=(i+a-1);
x1=roomlen-b+1;
x2=roomlen;
y11=i;
y22=(i+a-1);
x11=roomlen-b+1-c;
x22=roomlen-b;
subarea=roomMap[[y1;;y2,x1;;x2]];
antarea=roomMap[[y11;;y22,x11;;x22]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
tmpcenter = getCenterFromRange[y1,y2,x1,x2];
tmpdistance=getDistance[cx,cy,tmpcenter [[1]],tmpcenter [[2]] ];

If[tmplastdis==0,tmplastdis=tmpdistance;,
If[tmplastdis<tmpdistance,Break[];,tmplastdis=tmpdistance;];
];

If[tmpdistance<=minDistance,
minDistance=tmpdistance;
storage={{y1,y2,x1,x2},{y11,y22,x11,x22}};
];
]
];

(*south iterattion*)
tmplastdis=0;
For[i=1,i+a-1<= roomlen,i++,
y1=roomwid-b+1;
y2=roomwid;
x1=i;
x2=i+a-1;
y11=roomwid-b+1-c;
y22=roomwid-b;
x11=i;
x22=i+a-1;
subarea=roomMap[[y1;;y2,x1;;x2]];
antarea=roomMap[[y11;;y22,x11;;x22]];
If[FreeQ[Flatten[subarea],n_/;n>0]&&
(FreeQ[Flatten[antarea],n_/;n>0&&n<FLAGANTAREA]),
tmpcenter = getCenterFromRange[y1,y2,x1,x2];
tmpdistance=getDistance[cx,cy,tmpcenter [[1]],tmpcenter [[2]] ];
If[tmplastdis==0,tmplastdis=tmpdistance;,
If[tmplastdis<tmpdistance,Break[];,tmplastdis=tmpdistance;];
];
If[tmpdistance<=minDistance,
minDistance=tmpdistance;
storage={{y1,y2,x1,x2},{y11,y22,x11,x22}};
];
]
];

If[minDistance != roomlen,
y1=storage[[1]][[1]];
y2=storage[[1]][[2]];
x1=storage[[1]][[3]];
x2=storage[[1]][[4]];
y11=storage[[2]][[1]];
y22=storage[[2]][[2]];
x11=storage[[2]][[3]];
x22=storage[[2]][[4]];

subarea=roomMap[[y1;;y2,x1;;x2]]=objid;
antarea=roomMap[[y11;;y22,x11;;x22]]=FLAGANTAREA;

,Print["no space left"];
];
](*close if ref is {} *)
](*close module*)
];(*close foo*)

End[ ]

EndPackage[ ]
