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
 * Description:  Helpful structure for the ASK-CTL library
 *
 * Date:         September 11, 1996
 *
 * Copyright 1996 by University of Aarhus, Denmark
 *)

(* bitarray.sml
 * 
 * Warning: the implementation is 'un-safe': no checks are made upon
 * under/overflow in deletion/insertion.
 *
 * Rep. of set S': b[i] = 1 iff (e_i \in S', S' \subset S = {e_0,..,e_{n-1}})
 *)

structure BITARRAY:
    sig
        type bitarray;
            
        val ASK'new_array : bool -> int -> bitarray; 
        (* 'new_array true n' returns full bit-array with n elements *)

        val ASK'is_empty : bitarray -> bool;
        val ASK'insert : bitarray * int -> unit
        val ASK'addto : bitarray * int list -> unit
        val ASK'delete : bitarray * int -> unit
        val ASK'subfrom : bitarray * int list -> unit

        val ASK'member : bitarray * int -> bool
        val ASK'subset : bitarray * int list -> bool

        val ASK'filter : (int -> bool) -> bitarray -> int list
        val ASK'extract : bitarray -> int list
    end =
    struct
        type bitarray = ByteArray.bytearray; 

        open Bits;
        open ByteArray;

        fun ASK'div_8 ASK'n = rshift ( ASK'n, 3);
        fun ASK'mod_8 ASK'n = andb ( 7, ASK'n);

        fun ASK'intersect (ASK'a,ASK'b) =  (* test if some bits in b are set in a *)
            case andb(ASK'a,ASK'b) of   
                0 => false
              | _ => true
                    
        fun ASK'new_array ASK'full ASK'n =
            let
                val ASK'mod8rest = ASK'mod_8 ASK'n
                (* calculate space need: *)
                val ASK'm = (ASK'div_8 ASK'n) + 
                    (case (ASK'mod8rest) of 
                         0 => 0 
                       | _ => 1);
                val ASK'ba = (ByteArray.array(ASK'm, if ASK'full then 255 else 0));
            in
                if ASK'full then (* clear superfluous bits: *)
                    (case (ASK'mod8rest) of 
                         0 => ()              
                       | _ => update(ASK'ba, (ASK'm-1), rshift(255, (8-ASK'mod8rest)));
                             ASK'ba)
                else ASK'ba
            end;

        fun ASK'is_empty ASK'b =
            let
                fun ASK'not_empty ~1 = false
                  | ASK'not_empty ASK'n =
                    ASK'intersect(255,ByteArray.sub(ASK'b,ASK'n)) orelse ASK'not_empty (ASK'n-1)
            in
                not( ASK'not_empty (length ASK'b-1))
            end;

        fun ASK'insert (ASK'b,ASK'n) =
            let
                val ASK'i = ASK'div_8 ASK'n;
            in
                update( ASK'b, ASK'i, orb( ByteArray.sub( ASK'b, ASK'i), lshift( 1, ASK'mod_8 ASK'n)))
            end;

        fun ASK'addto ( _, []) = ()
          | ASK'addto ( ASK'b, ASK'e::ASK'xs) =
            ASK'insert(ASK'b,ASK'e) before ASK'addto(ASK'b, ASK'xs);

        fun ASK'delete( ASK'b, ASK'n) =
            let
                val ASK'i = ASK'div_8 ASK'n
            in
                update( ASK'b, ASK'i, xorb( ByteArray.sub(ASK'b,ASK'i), lshift(1, ASK'mod_8 ASK'n)))
            end;

        fun ASK'subfrom(_,[]) = ()
          | ASK'subfrom (ASK'b, ASK'e::ASK'xs) =
            ASK'delete(ASK'b,ASK'e) before ASK'subfrom( ASK'b, ASK'xs);

        fun ASK'member (ASK'b,ASK'n) = ASK'intersect(ByteArray.sub(ASK'b, ASK'div_8 ASK'n), lshift( 1, ASK'mod_8 ASK'n))
            
        fun ASK'subset (_,[]) = true
          | ASK'subset ( ASK'b, ASK'n::ASK'xs) =
            ASK'intersect(ByteArray.sub( ASK'b, ASK'div_8 ASK'n), lshift( 1, ASK'mod_8 ASK'n)) andalso
            ASK'subset(ASK'b,ASK'xs)

        fun ASK'filter ASK'f ASK'b =
            let
                val ASK'bl = length ASK'b;

                (* filter a single byte: *)
                fun ASK'filter_byte ( 1, ASK'n, ASK'elm, ASK'res) =
                    if andb(ASK'elm,1) = 1 andalso (ASK'f ASK'n) then ASK'n::ASK'res else ASK'res
                  | ASK'filter_byte ( ASK'i, ASK'n, ASK'elm, ASK'res) =
                    if andb(ASK'elm,ASK'i) = ASK'i andalso (ASK'f ASK'n) then
                         ASK'filter_byte( rshift(ASK'i,1), ASK'n-1, ASK'elm, ASK'n::ASK'res)
                    else
                        ASK'filter_byte( rshift(ASK'i,1), ASK'n-1, ASK'elm, ASK'res)

                fun ASK'filter' (_, ~1, ASK'res) = ASK'res
                  | ASK'filter' (ASK'n, ASK'byte_no, ASK'res) =
                    ASK'filter' (ASK'n-8, ASK'byte_no-1,
                              ASK'filter_byte ( 128, ASK'n, ByteArray.sub(ASK'b,ASK'byte_no), ASK'res))
            in
                ASK'filter' ( 8*ASK'bl-1, ASK'bl-1, [])
            end;
            
        fun ASK'extract ASK'b =
            let
                val ASK'bl = length ASK'b;
                    
                (* extract from a single byte: *)
                fun ASK'extract_byte ( 1, ASK'n, ASK'elm, ASK'res) =
                    (case andb(ASK'elm,1) of 0 => ASK'res | _ => ASK'n::ASK'res)
                  | ASK'extract_byte ( ASK'i, ASK'n, ASK'elm, ASK'res) =
                    (case andb(ASK'elm,ASK'i) of 0 =>
                         ASK'extract_byte( rshift(ASK'i,1), ASK'n-1, ASK'elm, ASK'res)
                       | _ => 
                             ASK'extract_byte( rshift(ASK'i,1), ASK'n-1, ASK'elm, ASK'n::ASK'res))

                fun ASK'extract' (_, ~1, ASK'res) = ASK'res
                  | ASK'extract' (ASK'n, ASK'byte_no, ASK'res) =
                    ASK'extract' (ASK'n-8, ASK'byte_no-1,
                              ASK'extract_byte ( 128, ASK'n, ByteArray.sub(ASK'b,ASK'byte_no), ASK'res))
            in
                ASK'extract' ( 8*ASK'bl-1, ASK'bl-1, [])
            end;

    end; (* BITARRAY *)
