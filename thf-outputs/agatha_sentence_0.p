module Agatha_Sentence_0
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

thf(e8_decl,type,(e8 : e)).
thf(e9_decl,type,(e9 : e)).
thf(e15_decl,type,(e15 : e)).
thf(e2_decl,type,(e2 : e)).
thf(e28_decl,type,(e28 : e)).
thf(e8_value,axiom,(e8 = (int_to_e @ 8))).
thf(e9_value,axiom,(e9 = (int_to_e @ 9))).
thf(e15_value,axiom,(e15 = (int_to_e @ 15))).
thf(e2_value,axiom,(e2 = (int_to_e @ 2))).
thf(e28_value,axiom,(e28 = (int_to_e @ 28))).
thf(id_Agatha_decl,type,id_Agatha: string).
thf(id_Aunt_decl,type,id_Aunt: string).
thf(id_Dreadbury_decl,type,id_Dreadbury: string).
thf(id_Mansion_decl,type,id_Mansion: string).
thf(h33_decl,type,h33: x > $o).
thf(h30_decl,type,h30: x > x > x > x > x > $o).
thf(h27_decl,type,h27: x > x > $o).
thf(h24_decl,type,h24: x > x > x > x > $o).
thf(h20_decl,type,h20: x > $o).
thf(h17_decl,type,h17: x > x > $o).
thf(h14_decl,type,h14: x > x > $o).
thf(h11_decl,type,h11: x > x > x > x > x > $o).
thf(h5_decl,type,h5: x > x > x > $o).
thf(h4_decl,type,h4: x > x > $o).
thf(h1_decl,type,h1: x > x > $o).
thf(h33,axiom,
   h33 = ( ^ [X29 : x] : (named @ X29 @ id_Aunt))).
thf(h30,axiom,
   h30 = ( ^ [X29 : x,X23 : x,X16 : x,X10 : x,X3 : x] : (proper_q @ X29 @ h33 @ (h11 @ X23 @ X16 @ X10 @ X3)))).
thf(h27,axiom,
   h27 = ( ^ [X29 : x,X23 : x] : ((named @ X23 @ id_Agatha) & (compound @ e28 @ X23 @ X29)))).
thf(h24,axiom,
   h24 = ( ^ [X29 : x,X23 : x,X3 : x,X10 : x] : (proper_q @ X23 @ (h27 @ X29) @ (h5 @ X10 @ X3)))).
thf(h20,axiom,
   h20 = ( ^ [X16 : x] : (named @ X16 @ id_Dreadbury))).
thf(h17,axiom,
   h17 = ( ^ [X16 : x,X10 : x] : (proper_q @ X16 @ h20 @ (h14 @ X10)))).
thf(h14,axiom,
   h14 = ( ^ [X10 : x,X16 : x] : ((named @ X10 @ id_Mansion) & (compound @ e15 @ X10 @ X16)))).
thf(h11,axiom,
   h11 = ( ^ [X23 : x,X16 : x,X10 : x,X3 : x,X29 : x] : (proper_q @ X10 @ (h17 @ X16) @ (h24 @ X29 @ X23 @ X3)))).
thf(h5,axiom,
   h5 = ( ^ [X10 : x,X3 : x,X23 : x] : (some_q @ X3 @ (h4 @ X10) @ (h1 @ X23)))).
thf(h4,axiom,
   h4 = ( ^ [X10 : x,X3 : x] : ((in_p_loc @ e9 @ e8 @ X10) & (live_v_1 @ e8 @ X3) & (person @ X3)))).
thf(h1,axiom,
   h1 = ( ^ [X23 : x,X3 : x] : (kill_v_1 @ e2 @ X3 @ X23))).
end Agatha_Sentence_0
