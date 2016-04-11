(************************************************************************)
(* CPN Tools                                                            *)
(* Copyright 2010-2011 AIS Group, Eindhoven University of Technology    *)
(*                                                                      *)
(* CPN Tools is originally developed by the CPN Group at Aarhus         *)
(* University from 2000 to 2010. The main architects behind the tool    *)
(* are Kurt Jensen, Soren Christensen, Lars M. Kristensen, and Michael  *)
(* Westergaard.  From the autumn of 2010, CPN Tools is transferred to   *)
(* the AIS group, Eindhoven University of Technology, The Netherlands.  *)
(*                                                                      *)
(* This file is part of CPN Tools.                                      *)
(*                                                                      *)
(* CPN Tools is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 2 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* CPN Tools is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with CPN Tools.  If not, see <http://www.gnu.org/licenses/>.   *)
(************************************************************************)
(*
 * Module:       CPN Tools State Space Tool
 *
 * Description:  Main part of the ASK-CTL library
 *
 * Date:         September 11, 1996
 *
 * Copyright 1996 by University of Aarhus, Denmark
 *)

structure ASKCTL :
sig
 type A
 val NF : (string * (Node -> bool)) -> A
 val AF : (string * (Arc -> bool)) -> A
 val TT : A
 val FF : A
 val NOT : A -> A
 val AND : (A * A) -> A
 val OR : (A * A) -> A
 val EXIST_NEXT : A -> A
 val FORALL_NEXT : A -> A
 val EXIST_UNTIL : (A * A) -> A
 val FORALL_UNTIL : (A * A) -> A
 val MODAL : A -> A
 val EXIST_MODAL : (A * A) -> A
 val FORALL_MODAL : (A * A) -> A
 val POS : A -> A
 val INV : A -> A
 val EV : A -> A
 val ALONG : A -> A
 val eval_node : A -> (Node -> bool) 
 val eval_arc : A -> (Arc -> bool) 
end = 
struct

	datatype Object =
	  STATE of Node
	| TRANS of Arc;
	fun exists _ [] = false
	  | exists CPN'f (CPN's::CPN'r) = (CPN'f CPN's) orelse (exists CPN'f CPN'r);
	fun forall CPN'f CPN'l = not (exists (not o CPN'f) CPN'l)

	fun filter _ [] = []
	  | filter CPN'p (CPN'h::CPN't) = if CPN'p CPN'h then CPN'h :: (filter CPN'p CPN't) else (filter CPN'p CPN't);
	fun NextCand (STATE CPN's) = map STATE (OutNodes CPN's)
	  | NextCand (TRANS CPN's) = map TRANS (OutArcs(DestNode CPN's))
	fun NextDual (STATE CPN's) = map TRANS (OutArcs CPN's)
	  | NextDual (TRANS CPN's) = [STATE (DestNode CPN's)]
	fun ObjectToNode (STATE CPN's) = CPN's
	  | ObjectToNode (TRANS _) = raise CPN'Stop "Error: Node handled as Arc in ASK-CTL";
	fun ObjectToArc (TRANS CPN's) = CPN's
	  | ObjectToArc (STATE _) = raise CPN'Stop "Error: Arc handled as Node in ASK-CTL";
	fun ObjectToString (TRANS CPN's) = mkst_Arc CPN's
	  | ObjectToString (STATE CPN's) = mkst_Node CPN's;
	fun ObjectToScc (TRANS CPN's) = NodeToScc (SourceNode CPN's)
	  | ObjectToScc (STATE CPN's) = NodeToScc CPN's;
	fun changeModal (TRANS _) = (STATE 1)
	  | changeModal (STATE _) = (TRANS 1);
	fun SccToFirstObject (TRANS _) scc = TRANS (hd (SccToArcs scc))
	  | SccToFirstObject (STATE _) scc = STATE (hd (SccToNodes scc));

    fun SccExistReachable (CPN's,CPN'pred)
       = let
          val thisscc = ObjectToScc CPN's
         in 
          if SccInitial thisscc
          then
           SearchSccs ([0],CPN'pred, 1, fn _ => (), false, fn _ => true)
          else 
           SearchReachableSccs (thisscc, CPN'pred, 1, fn _ => (), false, fn _ => true)
         end;

    fun SccForAllReachable (CPN's,CPN'pred)
       = not (SccExistReachable (CPN's,not o CPN'pred))

    fun SccExistReachableTerminal (CPN's,CPN'pred)
       = let
          val thisscc = ObjectToScc CPN's
         in
           if SccInitial thisscc
          then
           exists CPN'pred (TerminalSccs ())
          else 
	       SearchReachableSccs
            (thisscc, fn scc => (SccTerminal scc) andalso (CPN'pred scc),
             1, fn _ => (), false, fn _ => true)
         end;

(*
    fun SccExistReachableTerminal (CPN's,CPN'pred)
       = (case TerminalSccs ()
	      of [scc] => CPN'pred scc
	       | _ =>
	         SearchReachableSccs
              (ObjectToScc CPN's,
               fn scc => (SccTerminal scc) andalso (CPN'pred scc),
               1, fn _ => (), false, fn _ => true));
*)
   
    fun CycleExists (scc, eval_a, STATE _)
       = SearchSubgraphCycles
          (SccToNodes scc ,fn node  => scc = (NodeToScc node) andalso (eval_a (STATE node)),
           fn _ => true,fn _ => true,1,fn _ => true,false,fn _ => true)
      | CycleExists (scc, eval_a, TRANS _)
       = SearchSubgraphCycles
          (SccToNodes scc ,fn node  => scc = (NodeToScc node),
           fn arc => eval_a (TRANS arc),fn _ => true,1,fn _ => true,false,fn _ => true);

    fun ExistsInList _ [] = false
      | ExistsInList eval_a (CPN'h::CPN't) = (eval_a CPN'h) orelse (ExistsInList eval_a CPN't);

    fun ExistsElement (CPN'scc, eval_a, STATE _)
       = ExistsInList (fn CPN'node => eval_a (STATE CPN'node)) (SccToNodes CPN'scc)
      | ExistsElement (CPN'scc, eval_a, TRANS _)
       = ExistsInList (fn CPN'arc => eval_a (TRANS CPN'arc)) (SccToArcs CPN'scc);


datatype A =
    NF of (string * (Node -> bool))
  | AF of (string * (Arc -> bool))
  | TT
  | NOT of A
  | OR of A * A
  | EXIST_UNTIL of A * A
  | FORALL_UNTIL of A * A
  | MODAL of A;

datatype A' =
    NF' of (int * (string * (Node -> bool)))
  | AF' of (int * (string * (Arc -> bool)))
  | TT'
  | NOT' of (int * A')
  | OR' of (int * (A' * A'))
  | EXIST_UNTIL' of (int * (A' * A'))
  | FORALL_UNTIL' of (int * (A' * A'))
  | MODAL' of (int * A');
  
  (* Derived operators *)
val FF = NOT TT;
fun AND (CPN'a1,CPN'a2) = NOT(OR(NOT CPN'a1,NOT CPN'a2))
and EXIST_MODAL (CPN'a1,CPN'a2) = MODAL (AND (CPN'a1,MODAL CPN'a2))
and EXIST_NEXT CPN'a = EXIST_MODAL (TT,CPN'a)
and FORALL_NEXT CPN'a = NOT (EXIST_NEXT (NOT CPN'a))
and FORALL_MODAL (CPN'a1,CPN'a2) = NOT (EXIST_MODAL (CPN'a1,NOT CPN'a2))
and POS CPN'a = EXIST_UNTIL (TT,CPN'a)
and INV CPN'a = NOT (POS (NOT CPN'a))
and EV CPN'a = FORALL_UNTIL (TT,CPN'a)
and ALONG CPN'a = NOT (EV (NOT CPN'a));

fun rewrite (NF CPN'a) _ = NF CPN'a
  | rewrite (AF CPN'a) _ = AF CPN'a
  | rewrite TT _ = TT

(* efficient start *)

  | rewrite (EXIST_UNTIL(TT,EXIST_UNTIL(TT,CPN'a))) CPN's
   = rewrite (EXIST_UNTIL(TT,CPN'a)) CPN's
  | rewrite (EXIST_UNTIL(TT,FORALL_UNTIL(TT,CPN'a))) CPN's
   = rewrite (EXIST_UNTIL(TT,CPN'a)) CPN's
  | rewrite (FORALL_UNTIL(TT,EXIST_UNTIL(TT,CPN'a))) CPN's
   = rewrite (EXIST_UNTIL(TT,CPN'a)) CPN's
  | rewrite (FORALL_UNTIL(TT,FORALL_UNTIL(TT,CPN'a))) CPN's
   = rewrite (FORALL_UNTIL(TT,CPN'a)) CPN's

 (* efficient stop *)

  | rewrite (NOT CPN'a) CPN'm
   = (case rewrite CPN'a CPN'm
      of (NOT red_a) => red_a
       | red_a => NOT red_a)
  | rewrite (OR(CPN'a,CPN'b)) CPN'm
   = (case rewrite CPN'a CPN'm
      of TT => TT
       | (NOT TT) => rewrite CPN'b CPN'm
       | red_a =>
        (case rewrite CPN'b CPN'm
         of TT => TT
          | (NOT TT) => red_a
          | red_b => OR(red_a,red_b)))
  | rewrite (EXIST_UNTIL(CPN'a,CPN'b)) CPN'm
   = (case rewrite CPN'a CPN'm
      of (NOT TT) => rewrite CPN'b CPN'm
       | red_a => 
        (case rewrite CPN'b CPN'm
         of (NOT TT) => NOT TT
          | red_b => EXIST_UNTIL(red_a,red_b)))
  | rewrite (FORALL_UNTIL(CPN'a,CPN'b)) CPN'm
   = (case rewrite CPN'a CPN'm
      of (NOT TT) => rewrite CPN'b CPN'm
       | red_a => 
        (case rewrite CPN'b CPN'm
         of (NOT TT) => NOT TT
          | red_b => FORALL_UNTIL(red_a,red_b)))
  | rewrite (MODAL CPN'a) CPN'm
   = (case (rewrite CPN'a (changeModal CPN'm), CPN'm)
      of (NOT TT, _) => NOT TT
       | (TT,TRANS _) => TT   (* A transtion will allways have a next state *)
       | (red_a,_) => MODAL red_a);


val CPN'array_size = ref 0;
val CPN'start_values = ref 0;
val CPN'start_quants = ref 0;

 datatype status = TRUE | FALSE | UNKN | INPR;

exception StoreErr;
local
 val StateStore = CPN'AvlTree.AvlNew () : BITARRAY.bitarray CPN'AvlTree.avltree
 val TransStore = CPN'AvlTree.AvlNew () : BITARRAY.bitarray CPN'AvlTree.avltree

 fun Insert CPN'index CPN'value CPN'key CPN'tab
    = let
       val CPN'barr = CPN'AvlTree.AvlLookup (CPN'tab,CPN'key)
      in
       BITARRAY.ASK'insert (CPN'barr, CPN'index);
       if CPN'value then BITARRAY.ASK'insert (CPN'barr, (!CPN'start_values)+CPN'index) else ()
      end

 fun value2status true = TRUE
   | value2status false = FALSE;

 fun Lookup CPN'index CPN'quant CPN'key CPN'tab
    = let
       val CPN'barr = CPN'AvlTree.AvlLookup (CPN'tab,CPN'key)
      in
       if (BITARRAY.ASK'member (CPN'barr,CPN'index)) (* Known *)
       then
        value2status (BITARRAY.ASK'member (CPN'barr,(!CPN'start_values)+CPN'index))
       else
        if CPN'quant andalso (BITARRAY.ASK'member (CPN'barr,(!CPN'start_quants)+CPN'index))
        then
         INPR
        else
         (BITARRAY.ASK'insert (CPN'barr,(!CPN'start_quants)+CPN'index);
          UNKN)
      end
       handle CPN'AvlTree.ExcAvlLookup => 
        let
         val CPN'barr = BITARRAY.ASK'new_array false (!CPN'array_size)
        in
         if CPN'quant then 
          BITARRAY.ASK'insert (CPN'barr,(!CPN'start_quants)+CPN'index)
         else ();
         CPN'AvlTree.AvlInsert CPN'tab (CPN'key,CPN'barr);
         UNKN
        end
in
 fun EmptyStore ()
    = (CPN'AvlTree.AvlReset TransStore;
       CPN'AvlTree.AvlReset StateStore); 
 
 fun value2status true = TRUE
   | value2status false = FALSE;

 fun status2value TRUE = true
   | status2value FALSE = false
   | status2value _ = raise StoreErr;

 fun StoreRes CPN'index CPN'res (STATE CPN'n) = 
     (Insert CPN'index CPN'res (mkst_Node CPN'n) StateStore; CPN'res)
   | StoreRes CPN'index CPN'res (TRANS CPN'n) =
     (Insert CPN'index CPN'res (mkst_Arc CPN'n) TransStore; CPN'res);
   
 fun IsKnown CPN'index CPN'quant (STATE CPN'n) = Lookup CPN'index CPN'quant (mkst_Node CPN'n) StateStore
   | IsKnown CPN'index CPN'quant (TRANS CPN'n) = Lookup CPN'index CPN'quant (mkst_Arc CPN'n) TransStore
end;

fun label (NF CPN'a) (CPN'size,CPN'qsize) =  ((CPN'size-1,CPN'qsize),NF' (CPN'size,CPN'a))
  | label (AF CPN'a) (CPN'size,CPN'qsize) = ((CPN'size-1,CPN'qsize),AF' (CPN'size,CPN'a))
  | label TT (CPN'size,CPN'qsize) = ((CPN'size,CPN'qsize),TT')
  | label (NOT CPN'a) (CPN'size,CPN'qsize)
   = let
      val ((CPN'size_res,CPN'qsize_res),CPN'a_res) = label CPN'a (CPN'size-1,CPN'qsize)
     in
      ((CPN'size_res,CPN'qsize_res), NOT' (CPN'size,CPN'a_res))
     end
  | label (OR (CPN'a1,CPN'a2)) (CPN'size,CPN'qsize)
   = let
      val ((CPN'size_res,CPN'qsize_res),CPN'a1_res) = label CPN'a1 (CPN'size-1,CPN'qsize);
      val ((CPN'size_res,CPN'qsize_res),CPN'a2_res) = label CPN'a2 (CPN'size_res,CPN'qsize_res)
     in
      ((CPN'size_res,CPN'qsize_res), OR' (CPN'size,(CPN'a1_res,CPN'a2_res)))
     end
  | label (EXIST_UNTIL (CPN'a1,CPN'a2)) (CPN'size,CPN'qsize)
   = let
      val ((CPN'size_res,CPN'qsize_res),CPN'a1_res) = label CPN'a1 (CPN'size,CPN'qsize-1);
      val ((CPN'size_res,CPN'qsize_res),CPN'a2_res) = label CPN'a2 (CPN'size_res,CPN'qsize_res)
     in
      ((CPN'size_res,CPN'qsize_res), EXIST_UNTIL' (CPN'qsize,(CPN'a1_res,CPN'a2_res)))
     end
  | label (FORALL_UNTIL (CPN'a1,CPN'a2)) (CPN'size,CPN'qsize)
   = let
      val ((CPN'size_res,CPN'qsize_res),CPN'a1_res) = label CPN'a1 (CPN'size,CPN'qsize-1);
      val ((CPN'size_res,CPN'qsize_res),CPN'a2_res) = label CPN'a2 (CPN'size_res,CPN'qsize_res)
     in
      ((CPN'size_res,CPN'qsize_res), FORALL_UNTIL' (CPN'qsize,(CPN'a1_res,CPN'a2_res)))
     end
  | label (MODAL CPN'a) (CPN'size,CPN'qsize)
   = let
      val ((CPN'size_res,CPN'qsize_res),CPN'a_res) = label CPN'a (CPN'size-1,CPN'qsize)
     in
      ((CPN'size_res,CPN'qsize_res), MODAL' (CPN'size,CPN'a_res))
     end;

fun sizes (NF _) = (1,0)
  | sizes (AF _) = (1,0)
  | sizes TT = (0,0)
  | sizes (NOT CPN'a)
   = let
      val (CPN'size,CPN'qsize) = sizes CPN'a
     in
      (CPN'size+1,CPN'qsize)
     end
  | sizes (OR (CPN'a1,CPN'a2))
   = let
      val (CPN'size1,CPN'qsize1) = sizes CPN'a1;
      val (CPN'size2,CPN'qsize2) = sizes CPN'a2
     in
      (CPN'size1+CPN'size2+1,CPN'qsize1+CPN'qsize2)
     end
  | sizes (EXIST_UNTIL (CPN'a1,CPN'a2))
   = let
      val (CPN'size1,CPN'qsize1) = sizes CPN'a1;
      val (CPN'size2,CPN'qsize2) = sizes CPN'a2
     in
      (CPN'size1+CPN'size2+1,CPN'qsize1+CPN'qsize2+1)
     end
  | sizes (FORALL_UNTIL (CPN'a1,CPN'a2))
   = let
      val (CPN'size1,CPN'qsize1) = sizes CPN'a1;
      val (CPN'size2,CPN'qsize2) = sizes CPN'a2
     in
      (CPN'size1+CPN'size2+1,CPN'qsize1+CPN'qsize2+1)
     end
  | sizes (MODAL CPN'a)
   = let
      val (CPN'size,CPN'qsize) = sizes CPN'a
     in
      (CPN'size+1,CPN'qsize)
     end;

fun labelformula CPN'a
   = let
      val (CPN'size,CPN'qsize) = sizes CPN'a
     in
      CPN'array_size := 3*CPN'size;
      CPN'start_values := CPN'size;
      CPN'start_quants := 2*CPN'size;
      #2 (label CPN'a (CPN'size,CPN'qsize))
     end;
      

fun
    (* efficient start *)

    (* POS(INV(NOT(A))) *)
    eval' (EXIST_UNTIL'(_,(TT',inv_not_a as (NOT'(_,EXIST_UNTIL'(_,(TT',_))))))) CPN's
   = SccExistReachableTerminal
       (CPN's, (eval' inv_not_a) o (SccToFirstObject CPN's))

    (* POS(ALONG(NOT(A))) *)
  | eval' (EXIST_UNTIL'(_,(TT',NOT'(_,FORALL_UNTIL'(_,(TT',CPN'a)))))) CPN's
   = SccExistReachable
       (CPN's,
        fn CPN'scc =>
          if (SccTrivial CPN'scc)
          then
            ((SccTerminal CPN'scc))
           andalso
            (not (eval' CPN'a (SccToFirstObject CPN's CPN'scc)))
          else
           CycleExists (CPN'scc, not o (eval' CPN'a), CPN's))

    (* EV(INV(NOT(A))) *)
  | eval' (FORALL_UNTIL'(_,(TT',inv_not_a as (NOT'(_,EXIST_UNTIL'(_,(TT',_))))))) CPN's 
   = SccForAllReachable  
      (CPN's,
       fn CPN'scc => 
        if SccTerminal CPN'scc
        then
	 eval' inv_not_a (SccToFirstObject CPN's CPN'scc)
        else
         if SccTrivial CPN'scc
         then
          true
         else
          eval' inv_not_a (SccToFirstObject CPN's CPN'scc))


  | 
      (* the rest is the basic stuff *)
  
      eval' (CPN'a as NF'(CPN'i,(CPN'name,CPN'f))) CPN's
   = (case IsKnown CPN'i false CPN's
      of UNKN => StoreRes CPN'i (CPN'f (ObjectToNode CPN's)) CPN's
       | CPN'stat => status2value CPN'stat)
   
  | eval' (CPN'a as AF' (CPN'i,(CPN'name,CPN'f))) CPN's
   = (case IsKnown CPN'i false CPN's
      of UNKN => StoreRes CPN'i (CPN'f (ObjectToArc CPN's)) CPN's
       | CPN'stat => status2value CPN'stat)

  | eval' TT' _ = true
  
  | eval' (CPN'a as (NOT' (CPN'i,CPN'a1))) CPN's
   = (case IsKnown CPN'i false CPN's
      of UNKN => StoreRes CPN'i (not (eval' CPN'a1 CPN's)) CPN's
       | CPN'stat => status2value CPN'stat)
  
  | eval' (CPN'a as OR' (CPN'i,(CPN'a1,CPN'a2))) CPN's
   = (case IsKnown CPN'i false CPN's
      of UNKN => StoreRes CPN'i ((eval' CPN'a1 CPN's) orelse (eval' CPN'a2 CPN's)) CPN's
       | CPN'stat => status2value CPN'stat)
  
  | eval' (CPN'a as EXIST_UNTIL' (CPN'i,(CPN'a1,CPN'a2))) CPN's
   = (case IsKnown CPN'i true CPN's
      of UNKN => StoreRes CPN'i 
                  ((eval' CPN'a2 CPN's) orelse
                   ((eval' CPN'a1 CPN's) andalso (exists (eval' CPN'a) (NextCand CPN's)))) CPN's
       | INPR => false
       | CPN'stat => status2value CPN'stat)
   
  | eval' (CPN'a as FORALL_UNTIL' (CPN'i,(CPN'a1,CPN'a2))) CPN's
   = (case IsKnown CPN'i true CPN's
      of UNKN => StoreRes CPN'i 
                  ((eval' CPN'a2 CPN's) orelse
                   ((eval' CPN'a1 CPN's) andalso (forall (eval' CPN'a) (NextCand CPN's)))) CPN's
       | INPR => false
       | CPN'stat => status2value CPN'stat)
   
  | eval' (CPN'a as MODAL' (CPN'i,CPN'a1)) CPN's
   = (case IsKnown CPN'i false CPN's
      of UNKN => StoreRes CPN'i  (exists (eval' CPN'a1) (NextDual CPN's)) CPN's
       | CPN'stat => status2value CPN'stat);
		     
fun eval_node CPN'a CPN's
   = (EmptyStore();
      eval' (labelformula (rewrite CPN'a (STATE 1))) (STATE CPN's));
fun eval_arc CPN'a CPN's
   = (EmptyStore();
      eval' (labelformula (rewrite CPN'a (TRANS 1))) (TRANS CPN's));
end;
