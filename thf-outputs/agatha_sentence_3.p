module Agatha_Sentence_3
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

thf(e2_decl,type,(e2 : e)).
thf(e15_decl,type,(e15 : e)).
thf(e21_decl,type,(e21 : e)).
thf(e29_decl,type,(e29 : e)).
thf(e2_value,axiom,(e2 = (int_to_e @ 2))).
thf(e15_value,axiom,(e15 = (int_to_e @ 15))).
thf(e21_value,axiom,(e21 = (int_to_e @ 21))).
thf(e29_value,axiom,(e29 = (int_to_e @ 29))).
thf(id_1_decl,type,id_1: string).
thf(id_Agatha_decl,type,id_Agatha: string).
thf(id_Charles_decl,type,id_Charles: string).
thf(h26_decl,type,h26: x > $o).
thf(h23_decl,type,h23: x > x > x > $o).
thf(h20_decl,type,h20: x > x > $o).
thf(h16_decl,type,h16: x > x > x > $o).
thf(h13_decl,type,h13: x > x > $o).
thf(h10_decl,type,h10: x > x > x > x > $o).
thf(h7_decl,type,h7: x > $o).
thf(h4_decl,type,h4: x > x > $o).
thf(h1_decl,type,h1: x > x > $o).
thf(h26,axiom,
   h26 = ( ^ [X22 : x] : (aunt_n_of @ X22))).
thf(h23,axiom,
   h23 = ( ^ [X22 : x,X17 : x,X9 : x] : (udef_q @ X22 @ h26 @ (h16 @ X17 @ X9)))).
thf(h20,axiom,
   h20 = ( ^ [X22 : x,X17 : x] : ((named @ X17 @ id_Agatha) & (compound @ e21 @ X17 @ X22)))).
thf(h16,axiom,
   h16 = ( ^ [X17 : x,X9 : x,X22 : x] : (proper_q @ X17 @ (h20 @ X22) @ (h13 @ X9)))).
thf(h13,axiom,
   h13 = ( ^ [X9 : x,X17 : x] : ((hate_v_1 @ e29 @ X17 @ X9) & (card @ e15 @ X9 @ id_1) & (generic_entity @ X9)))).
thf(root,axiom,
   root = ( ^ [X22 : x,X17 : x,X9 : x,X3 : x] : (no_q @ X9 @ (h23 @ X22 @ X17) @ (h4 @ X3)))).
thf(h7,axiom,
   h7 = ( ^ [X3 : x] : (named @ X3 @ id_Charles))).
thf(h4,axiom,
   h4 = ( ^ [X3 : x,X9 : x] : (proper_q @ X3 @ h7 @ (h1 @ X9)))).
thf(h1,axiom,
   h1 = ( ^ [X9 : x,X3 : x] : (hate_v_1 @ e2 @ X3 @ X9))).
end Agatha_Sentence_3
