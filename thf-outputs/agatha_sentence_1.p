module Agatha_Sentence_1
thf(x_decl,type,x : $tType).
thf(e_decl,type,e : $tType).
thf(string_decl,type,string : $i).
thf(int_to_e_decl,type,int_to_e: $int > e).

thf(every_q_decl,type,every_q:               x > (x > $o) > (x > $o) > $o).
thf(some_q_decl,type,some_q:                 x > (x > $o) > (x > $o) > $o).
thf(the_q_decl,type,the_q:                   x > (x > $o) > (x > $o) > $o).
thf(proper_q_decl,type,proper_q:             x > (x > $o) > (x > $o) > $o).
thf(pronoun_q_decl,type,pronoun_q:           x > (x > $o) > (x > $o) > $o).
thf(udef_q_decl,type,udef_q:                 x > (x > $o) > (x > $o) > $o).
thf(def_explicit_q_decl,type,def_explicit_q: x > (x > $o) > (x > $o) > $o).
thf(no_q_decl,type,no_q:                     x > (x > $o) > (x > $o) > $o).
thf(never_a_1_decl,type,never_a_1:           ($o) > $o).
thf(neg_decl,type,neg:                       e > ($o) > $o).
thf(colon_p_namely,type,colon_p_namely:      e > ($o) > ($o) > $o).

thf(therein_p_dir_decl,type,therein_p_dir: e > e > $o).
thf(live_v_1_decl,type,live_v_1: e > x > $o).
thf(people_n_of_decl,type,people_n_of: x > $o).
thf(vicitm_n_of_decl,type,victim_n_of: x > $o).
thf(only_a_1_decl,type,only_a_1: e > x > $o).
thf(named_decl,type,named: x > string > $o).
thf(and_c_x_decl,type,and_c_x: x > x > x > $o).
thf(and_c_e_decl,type,and_c_e: e > e > e > $o).
thf(butler_n_1_decl,type,butler_n_1: x > $o).
thf(killer_n_1_decl,type,killer_n_1: x > $o).
thf(implicit_conj_decl,type,implicit_conj: x > x > x > $o).
thf(be_v_id_decl,type,be_v_id: e > x > x > $o).
thf(in_p_loc_decl,type,in_p_loc: e > e > x > $o).
thf(compound_decl,type,compound: e > x > x > $o).
thf(person_decl,type,person: x > $o).
thf(kill_v_1_decl,type,kill_v_1: e > x > x > $o).
thf(hate_v_1_decl,type,hate_v_1: e > x > x > $o).
thf(pron_decl,type,pron: x > $o).
thf(poss,type,poss: e > x > x > $o).
thf(more_comp,type,more_comp: e > e > x > $o).
thf(rich_a_in,type,rich_a_in: e > x > $o).
thf(always_a_1,type,always_a_1: e > $o).
thf(aunt_n_of,type,aunt_n_of: x > $o).
thf(card,type,card: e > x > string > $o).
thf(generic_entity,type,generic_entity: x > $o).
thf(except_p,type,except_p: e > x > x > $o).
thf(therefore_a_1,type,therefore_a_1: ($o) > $o).
thf(unknown,type,unknown: e > $o).

thf(e30_decl,type,(e30 : e)).
thf(e31_decl,type,(e31 : e)).
thf(e37_decl,type,(e37 : e)).
thf(e45_decl,type,(e45 : e)).
thf(e2_decl,type,(e2 : e)).
thf(e51_decl,type,(e51 : e)).
thf(e53_decl,type,(e53 : e)).
thf(e54_decl,type,(e54 : e)).
thf(e30_value,axiom,(e30 = (int_to_e @ 30))).
thf(e31_value,axiom,(e31 = (int_to_e @ 31))).
thf(e37_value,axiom,(e37 = (int_to_e @ 37))).
thf(e45_value,axiom,(e45 = (int_to_e @ 45))).
thf(e2_value,axiom,(e2 = (int_to_e @ 2))).
thf(e51_value,axiom,(e51 = (int_to_e @ 51))).
thf(e53_value,axiom,(e53 = (int_to_e @ 53))).
thf(e54_value,axiom,(e54 = (int_to_e @ 54))).
thf(id_Agatha_decl,type,id_Agatha: string).
thf(id_Charles_decl,type,id_Charles: string).
thf(id_Dreadbury_decl,type,id_Dreadbury: string).
thf(id_Mansion_decl,type,id_Mansion: string).
thf(h50_decl,type,h50: x > $o).
thf(h47_decl,type,h47: x > x > x > x > x > $o).
thf(h42_decl,type,h42: x > $o).
thf(h39_decl,type,h39: x > x > $o).
thf(h36_decl,type,h36: x > x > $o).
thf(h33_decl,type,h33: x > x > x > x > x > x > x > x > $o).
thf(h28_decl,type,h28: x > $o).
thf(h25_decl,type,h25: x > x > x > $o).
thf(h23_decl,type,h23: x > x > x > $o).
thf(h22_decl,type,h22: x > $o).
thf(h18_decl,type,h18: x > x > x > $o).
thf(h17_decl,type,h17: x > x > x > $o).
thf(h13_decl,type,h13: x > x > x > x > x > x > x > $o).
thf(h11_decl,type,h11: x > $o).
thf(h7_decl,type,h7: x > x > x > $o).
thf(h4_decl,type,h4: x > x > x > x > x > $o).
thf(h1_decl,type,h1: x > x > x > $o).
thf(h50,axiom,
   h50 = ( ^ [X46 : x] : ((therein_p_dir @ e54 @ e53) & (live_v_1 @ e53 @ X46) & (people_n_of @ X46) & (only_a_1 @ e51 @ X46)))).
thf(h47,axiom,
   h47 = ( ^ [X46 : x,X32 : x,X8 : x,X3 : x,X14 : x] : (the_q @ X46 @ h50 @ (h4 @ X32 @ X14 @ X8 @ X3)))).
thf(h42,axiom,
   h42 = ( ^ [X38 : x] : (named @ X38 @ id_Dreadbury))).
thf(h39,axiom,
   h39 = ( ^ [X38 : x,X32 : x] : (proper_q @ X38 @ h42 @ (h36 @ X32)))).
thf(h36,axiom,
   h36 = ( ^ [X32 : x,X38 : x] : ((named @ X32 @ id_Mansion) & (compound @ e37 @ X32 @ X38)))).
thf(h33,axiom,
   h33 = ( ^ [X46 : x,X38 : x,X32 : x,X24 : x,X19 : x,X14 : x,X8 : x,X3 : x] : (proper_q @ X32 @ (h39 @ X38) @ (h13 @ X46 @ X24 @ X19 @ X14 @ X8 @ X3)))).
thf(h28,axiom,
   h28 = ( ^ [X24 : x] : (named @ X24 @ id_Charles))).
thf(h25,axiom,
   h25 = ( ^ [X24 : x,X19 : x,X14 : x] : (proper_q @ X24 @ h28 @ (h18 @ X19 @ X14)))).
thf(h23,axiom,
   h23 = ( ^ [X24 : x,X14 : x,X19 : x] : (and_c_x @ X14 @ X19 @ X24))).
thf(h22,axiom,
   h22 = ( ^ [X19 : x] : (butler_n_1 @ X19))).
thf(h18,axiom,
   h18 = ( ^ [X19 : x,X14 : x,X24 : x] : (the_q @ X19 @ h22 @ (h23 @ X24 @ X14)))).
thf(h17,axiom,
   h17 = ( ^ [X14 : x,X3 : x,X8 : x] : (implicit_conj @ X3 @ X8 @ X14))).
thf(h13,axiom,
   h13 = ( ^ [X46 : x,X24 : x,X19 : x,X14 : x,X8 : x,X3 : x,X32 : x] : (udef_q @ X14 @ (h25 @ X24 @ X19) @ (h47 @ X46 @ X32 @ X8 @ X3)))).
thf(h11,axiom,
   h11 = ( ^ [X8 : x] : (named @ X8 @ id_Agatha))).
thf(h7,axiom,
   h7 = ( ^ [X14 : x,X8 : x,X3 : x] : (proper_q @ X8 @ h11 @ (h17 @ X14 @ X3)))).
thf(h4,axiom,
   h4 = ( ^ [X32 : x,X14 : x,X8 : x,X3 : x,X46 : x] : (udef_q @ X3 @ (h7 @ X14 @ X8) @ (h1 @ X46 @ X32)))).
thf(h1,axiom,
   h1 = ( ^ [X46 : x,X32 : x,X3 : x] : ((be_v_id @ e45 @ X3 @ X46) & (and_c_e @ e2 @ e30 @ e45) & (in_p_loc @ e31 @ e30 @ X32) & (live_v_1 @ e30 @ X3)))).
end Agatha_Sentence_1
