(* Author: Rolf Mertig *)
(* Organization: GluonVision GmbH *)
(* License: LGPL *)
(* Copyright: Rolf Mertig*)
(* Date: 2013-02-20 *)

(* Documentation: Evaluate remotely (by ssh and vnc) from a command line a saved notebook.
   For a sample usage see the Example section at the end of this file.
   
   This file by default evaluates test.nb which should be placed in the same directory as this
   file. See the Example section at the end.

*)

(* Limitations: This is tested on Linux, Mathematica 7 or 9, using tigervnc and ssh *)

(* Version: 2.0 *)

(* History: based on http://www.gluonvision.com/downloads/NB2PDF.m *)

(* Mathematica version needed: 7.0 or newer *)

(* ************************************* *)

(* This code should not be run from the FrontEnd ... : *)
If[$FrontEnd =!= Null,
   Print["This program should be run from the Kernel, not from a FrontEnd."] ,
(* ELSE *)


BeginPackage["EvalNB`"];

$Debug::usage="If set to True debugging messages are printed."; ;

EvalNB::usage="EvalNB[nbfile] evaluates the notebook nbfile and saves the evaluated notebook.";

NBOutToken::usage="NBOutToken is an option for EvalNB. Its setting determines the string token added before the .nb starting filename. The resulting string is used as name for the evaluated saved notebook.";

Begin["`Private`"];

$Debug = False;

Options[EvalNB] = {NBOutToken -> "_Evaluated", Pause -> 0.3};

EvalNB[nb_String /; StringMatchQ[nb, "*.nb", IgnoreCase-> True], 
       opts___?OptionQ ] :=  
       evalnb[Get[nb], nb, opts] /; FileNames[nb] =!= {};
       (* Get might, though it should not, give problems sometimes. 
          Try Import then or fix the notebook or send the nb to WRI 
          and ask them to fix it. *)

evalnb[nbin_Notebook, nbfilename_String, opts___?OptionQ] := 
        Module[{nb, nbtmp, nt, nbfout, pause, nbtoken, fsizecheck, fsizecheck2, ftype},

nbtmp = $TemporaryPrefix <> StringJoin @@ Map[ToString, Date[]] <>".txt";
If[FileNames[nbtmp] =!= {}, DeleteFile @ nbtmp];
nbtoken = NBOutToken /. {opts} /. Options[EvalNB];
pause = Pause /. {opts} /. Options[EvalNB];
nbfout = StringInsert[nbfilename, nbtoken, -4];
(* in order to tell the FE that the notebook really exists we need to 
   create something on disk. If this is not done a strange FE-dialog
   appears ...
*)
nbtmp = $TemporaryPrefix <> StringJoin @@ Map[ToString, Date[]] <>".txt";
If[FileNames[nbtmp] =!= {}, DeleteFile @ nbtmp];
nbtoken = NBOutToken /. {opts} /. Options[EvalNB];
pause = Pause /. {opts} /. Options[EvalNB];
nbfout = StringInsert[nbfilename, nbtoken, -4];
(* in order to tell the FE that the notebook really exists we need to 
   create something on disk. If this is not done a strange FE-dialog
   appears ...
*)
If[FileNames[nbfout] === {}, Export[nbfout,""]];

(* we need JLink *)
Needs["JLink`"];

JLink`InstallJava[];

(* this uses the FrontEnd as a service, in the background *)
JLink`UseFrontEnd[
nt = nbin;
(* add a final temporary cell and open the notebook : *)
nb = NotebookPut @ Insert[nt, 
       Cell[ "Export[\"" <> nbtmp <> 
             "\"," <> ToString[Date[]]<> ",\"Text\"];", "Input"], {1,-1}];
(* select all cells *)
SelectionMove[nb, Before, Notebook];
SelectionMove[nb,All,Notebook];
(* Evaluate everything. Attention: the notebook should terminate ... *)
SelectionEvaluate[nb];
If[$Debug, Print["starting While "]];
(* This is the tricky part: 
   Since there are really two programs, the FrontEnd and the Kernel,
   we have to tell the kernel to wait for the FrontEnd to finish 
   the evaluation of the notebook.
   We put the kernel evaluator in a While - loop until the last 
   cell of the original notebook is evaluated (which cannot be done 
   somehow with a kernel value or another FrontEnd-action, so we 
   resume to writing a temporary file to disk and check if it there)
*)

While[ (ftype = FileType[nbtmp]) =!= File, 
	    If[$Debug, Print["While waiting: ",  ftype]]; Pause[pause]
	    ];

If[$Debug, Print["finished While "] ];

(* at this point we are sure that everything is finished and the 
   extra cell as well as the temporary file are removed easily:
*)
SelectionMove[nb, After, Notebook]; SelectionMove[nb, Previous, Cell];
DeleteFile[nbtmp]; Pause[pause]; (* *)
NotebookDelete[nb]; Pause[pause]; (* *)
NotebookSave[nb, nbfout, Interactive -> False];
(* make sure file size does not change anymore *)
fsizecheck = fsizecheck2 = 0;
While[fsizecheck =!= fsizecheck2 || fsizecheck === 0,
      fsizecheck = FileByteCount[nbfout];
      Print["fsizecheck = ",fsizecheck] /; $Debug;
      Pause[pause];
      fsizecheck2 = FileByteCount[nbfout];
     ];
Pause[pause];
];
];

End[];

EndPackage[];
];

(* ******************************************************* *)

(* EXAMPLE:  *) 

(* ******************************************************* *)

(* Example usage:


   Prerequisites:

I )
   You need to have some vncserver running on the remote server.
   This can easily be done, e.g., on Fedora Linux, by:

0.: install tigervnc-server on the remote server and tigervnc on your local machine, e.g. by: 
    yum -y install tigervnc-server tigervnc
1.: On your remote machine, do:
2.: sudo cp /lib/systemd/system/vncserver@.service /etc/systemd/system/multi-user.target.wants/vncserver@:1.service
3.: Edit the vncserver@:1.service file as root, replacing <USER> with your username (not root).  
    On the line that says 'ExecStart', add '-geometry 1280x720' before the '%i', without the ' quotes. Save the file.
4.: In a terminal as your user, not root, run vncpasswd and set the VNC login password for your user.
5.: Run 'sudo systemctl start vncserver@:1.service'
6.: Eventually run systemctl daemon-reload
   
II)
   Now, for tunneling VNC through ssh from your local Linux machine, 
   so only ssh port has to be open in the firewall, do 
   (change your user for rolfm and your remote server IP for 192.168.2.104):
   ssh -L 5901:localhost:5901 -N -f -l rolfm 192.168.2.104

   Then, run:
     vncviewer localhost:1

   Now, in the 'remote desktop' opened by vncviewer start a konsole (xterm),  cd
   to the directory where EvalNB.m and test.nb are and do:

     math < EvalNB.m > EvalNB.out &

   You may or may not log out from that konsole (or xterm).
   You can now close the 'remote desktop' vncviewer and have a coffee.

   Once everything is finished the evaluated notebook will be saved as
   test_Evaluated.nb

*)

SetDirectory[DirectoryName[System`Private`FindFile[$Input]]];
(* ******************************************************************** *)


(* assume that test.nb is in the same directory as EvalNB.m *)
(* 
E.g., generate a test.nb by this: 

Notebook[{Cell[BoxData["$Version"], "Input"], Cell[BoxData[\(DateString[]\)], 
   "Input"], Cell[BoxData[\(Plot[\(x, \({x, 0, 12}\)\)]\)], "Input"], Cell[
   BoxData[\(\(Pause[21]\) ;\)], "Input"], Cell[BoxData[\(DateString[]\)], 
   "Input"], Cell[BoxData[\(Expand[\(\((a - b - c - d - e)\)^6\)]\)], "Input"]}
] >> "test.nb"
*)
(* change test.nb to the notebook you want to evaluate *)
EvalNB @ "test.nb"
