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

thf(e9_decl,type,(e9 : e)).
thf(e15_decl,type,(e15 : e)).
thf(e22_decl,type,(e22 : e)).
thf(e2_decl,type,(e2 : e)).
thf(e27_decl,type,(e27 : e)).
thf(e33_decl,type,(e33 : e)).
thf(e9_value,axiom,(e9 = (int_to_e @ 9))).
thf(e15_value,axiom,(e15 = (int_to_e @ 15))).
thf(e22_value,axiom,(e22 = (int_to_e @ 22))).
thf(e2_value,axiom,(e2 = (int_to_e @ 2))).
thf(e27_value,axiom,(e27 = (int_to_e @ 27))).
thf(e33_value,axiom,(e33 = (int_to_e @ 33))).
thf(h38_decl,type,h38: x > $o).
thf(h35_decl,type,h35: x > x > x > x > $o).
thf(h32_decl,type,h32: x > x > $o).
thf(h29_decl,type,h29: x > x > x > $o).
thf(h25_decl,type,h25: x > x > $o).
thf(h20_decl,type,h20: x > $o).
thf(h17_decl,type,h17: x > x > $o).
thf(h14_decl,type,h14: x > x > $o).
thf(h11_decl,type,h11: x > x > x > x > x > $o).
thf(h7_decl,type,h7: x > $o).
thf(h4_decl,type,h4: x > x > x > x > $o).
thf(h1_decl,type,h1: x > x > x > x > $o).
thf(h38,axiom,
   h38 = ( ^ [X34 : x] : (pron @ X34))).
thf(h35,axiom,
   h35 = ( ^ [X34 : x,X28 : x,X3 : x,X10 : x] : (pronoun_q @ X34 @ h38 @ (h4 @ X28 @ X10 @ X3)))).
thf(h32,axiom,
   h32 = ( ^ [X34 : x,X28 : x] : ((victim_n_of @ X28) & (poss @ e33 @ X28 @ X34)))).
thf(h29,axiom,
   h29 = ( ^ [X34 : x,X28 : x,X3 : x] : (def_explicit_q @ X28 @ (h32 @ X34) @ (h25 @ X3)))).
thf(h25,axiom,
   h25 = ( ^ [X3 : x,X28 : x] : ((more_comp @ e27 @ e22 @ X28) & (rich_a_in @ e22 @ X3)))).
thf(h20,axiom,
   h20 = ( ^ [X16 : x] : (pron @ X16))).
thf(h17,axiom,
   h17 = ( ^ [X16 : x,X10 : x] : (pronoun_q @ X16 @ h20 @ (h14 @ X10)))).
thf(h14,axiom,
   h14 = ( ^ [X10 : x,X16 : x] : ((victim_n_of @ X10) & (poss @ e15 @ X10 @ X16)))).
thf(h11,axiom,
   h11 = ( ^ [X34 : x,X28 : x,X16 : x,X10 : x,X3 : x] : (def_explicit_q @ X10 @ (h17 @ X16) @ (h35 @ X34 @ X28 @ X3)))).
thf(h7,axiom,
   h7 = ( ^ [X3 : x] : (killer_n_1 @ X3))).
thf(h4,axiom,
   h4 = ( ^ [X28 : x,X10 : x,X3 : x,X34 : x] : (a_q @ X3 @ h7 @ (h1 @ X34 @ X28 @ X10)))).
thf(h1,axiom,
   h1 = ( ^ [X34 : x,X28 : x,X10 : x,X3 : x] : ((never_a_1 @ (h29 @ X34 @ X28 @ X3)) & (and_c_e @ e2 @ e9 @ e22) & (hate_v_1 @ e9 @ X3 @ X10) & (always_a_1 @ e9)))).