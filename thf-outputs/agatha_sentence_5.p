module Agatha_Sentence_5
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
thf(e13_decl,type,(e13 : e)).
thf(e16_decl,type,(e16 : e)).
thf(e18_decl,type,(e18 : e)).
thf(e24_decl,type,(e24 : e)).
thf(e2_value,axiom,(e2 = (int_to_e @ 2))).
thf(e13_value,axiom,(e13 = (int_to_e @ 13))).
thf(e16_value,axiom,(e16 = (int_to_e @ 16))).
thf(e18_value,axiom,(e18 = (int_to_e @ 18))).
thf(e24_value,axiom,(e24 = (int_to_e @ 24))).
thf(id_Agatha_decl,type,id_Agatha: string).
thf(id_Aunt_decl,type,id_Aunt: string).
thf(h29_decl,type,h29: x > $o).
thf(h26_decl,type,h26: x > x > $o).
thf(h23_decl,type,h23: x > x > $o).
thf(h20_decl,type,h20: x > x > x > $o).
thf(h15_decl,type,h15: x > x > $o).
thf(h10_decl,type,h10: x > x > x > x > $o).
thf(h9_decl,type,h9: x > x > x > $o).
thf(h7_decl,type,h7: x > $o).
thf(h4_decl,type,h4: x > x > $o).
thf(h1_decl,type,h1: x > x > $o).
thf(h29,axiom,
   h29 = ( ^ [X25 : x] : (named @ X25 @ id_Aunt))).
thf(h26,axiom,
   h26 = ( ^ [X25 : x,X19 : x] : (proper_q @ X25 @ h29 @ (h23 @ X19)))).
thf(h23,axiom,
   h23 = ( ^ [X19 : x,X25 : x] : ((named @ X19 @ id_Agatha) & (compound @ e24 @ X19 @ X25)))).
thf(h20,axiom,
   h20 = ( ^ [X25 : x,X19 : x,X8 : x] : (proper_q @ X19 @ (h26 @ X25) @ (h15 @ X8)))).
thf(h15,axiom,
   h15 = ( ^ [X8 : x,X19 : x] : ((more_comp @ e18 @ e16 @ X19) & (rich_a_in @ e16 @ X8)))).
thf(h10,axiom,
   h10 = ( ^ [X25 : x,X19 : x,X8 : x,X3 : x] : (every_q @ X8 @ (h9 @ X25 @ X19) @ (h4 @ X3)))).
thf(h9,axiom,
   h9 = ( ^ [X25 : x,X19 : x,X8 : x] : ((neg @ e13 @ (h20 @ X25 @ X19 @ X8)) & (person @ X8)))).
thf(h7,axiom,
   h7 = ( ^ [X3 : x] : (butler_n_1 @ X3))).
thf(h4,axiom,
   h4 = ( ^ [X3 : x,X8 : x] : (the_q @ X3 @ h7 @ (h1 @ X8)))).
thf(h1,axiom,
   h1 = ( ^ [X8 : x,X3 : x] : (hate_v_1 @ e2 @ X3 @ X8))).
end Agatha_Sentence_5
